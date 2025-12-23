(ns clojure-mcp-light.java-source
  "Locate and extract Java sources from JDK src.zip or library sources JARs.

  Supports:
  - JDK classes (java.*, javax.*, sun.*) from src.zip
  - Library classes from -sources.jar files
  - Decompilation via CFR when no source JAR is available"
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure-mcp-light.decompiler :as decompiler]
            [clojure-mcp-light.jar-extract :as jar]
            [clojure-mcp-light.nrepl-client :as nc]
            [clojure-mcp-light.tmp :as tmp]))

;; ============================================================================
;; Class Classification
;; ============================================================================

(defn classify-java-class
  "Classify a Java class as :jdk or :library based on package name.

  JDK classes: java.*, javax.*, sun.*, jdk.*
  Everything else: :library"
  [class-name]
  (cond
    (or (str/starts-with? class-name "java.")
        (str/starts-with? class-name "javax.")
        (str/starts-with? class-name "sun.")
        (str/starts-with? class-name "jdk.")
        (str/starts-with? class-name "com.sun."))
    :jdk

    :else
    :library))

(defn class-to-source-path
  "Convert a class name to its expected source file path.

  java.util.HashMap -> java/util/HashMap.java"
  [class-name]
  (str (str/replace class-name "." "/") ".java"))

(defn class-to-module-prefix
  "Get the JDK module prefix for a class (JDK 9+ src.zip structure).

  java.util.HashMap -> java.base
  javax.swing.JFrame -> java.desktop

  Returns nil for unknown packages (will try without prefix)."
  [class-name]
  (cond
    ;; java.base module
    (or (str/starts-with? class-name "java.lang.")
        (str/starts-with? class-name "java.util.")
        (str/starts-with? class-name "java.io.")
        (str/starts-with? class-name "java.nio.")
        (str/starts-with? class-name "java.net.")
        (str/starts-with? class-name "java.math.")
        (str/starts-with? class-name "java.text.")
        (str/starts-with? class-name "java.time.")
        (str/starts-with? class-name "java.security.")
        (str/starts-with? class-name "java.reflect."))
    "java.base"

    ;; java.sql module
    (or (str/starts-with? class-name "java.sql.")
        (str/starts-with? class-name "javax.sql."))
    "java.sql"

    ;; java.desktop module
    (or (str/starts-with? class-name "java.awt.")
        (str/starts-with? class-name "javax.swing.")
        (str/starts-with? class-name "javax.imageio."))
    "java.desktop"

    ;; java.logging module
    (str/starts-with? class-name "java.util.logging.")
    "java.logging"

    ;; java.xml module
    (or (str/starts-with? class-name "javax.xml.")
        (str/starts-with? class-name "org.xml.")
        (str/starts-with? class-name "org.w3c."))
    "java.xml"

    ;; jdk.* classes
    (str/starts-with? class-name "jdk.")
    "jdk.internal"

    ;; sun.* classes - usually in java.base
    (str/starts-with? class-name "sun.")
    "java.base"

    ;; Default - try java.base first
    :else nil))

(defn class-to-source-paths
  "Get possible source file paths for a class.

  For JDK 9+, returns both module-prefixed and non-prefixed paths.
  Returns vector of paths to try in order."
  [class-name]
  (let [base-path (class-to-source-path class-name)
        module (class-to-module-prefix class-name)]
    (if module
      [(str module "/" base-path) base-path]
      [base-path])))

(defn class-to-class-path
  "Convert a class name to its expected class file path.

  java.util.HashMap -> java/util/HashMap.class"
  [class-name]
  (str (str/replace class-name "." "/") ".class"))

;; ============================================================================
;; JDK Source Location
;; ============================================================================

(defn find-jdk-src-zip
  "Locate JDK src.zip from JAVA_HOME or common locations.

  Checks in order:
  1. $JAVA_HOME/lib/src.zip (JDK 9+)
  2. $JAVA_HOME/src.zip (JDK 8)
  3. SDKMAN: ~/.sdkman/candidates/java/current/lib/src.zip

  Returns path to src.zip or nil if not found."
  []
  (let [java-home (System/getenv "JAVA_HOME")
        candidates (cond-> []
                     java-home (conj (str java-home "/lib/src.zip")
                                     (str java-home "/src.zip"))
                     true (conj (str (System/getProperty "user.home")
                                     "/.sdkman/candidates/java/current/lib/src.zip")))]
    (first (filter fs/exists? candidates))))

(defn get-jdk-version
  "Get JDK version string for cache organization.

  Uses java.version system property, extracting major version.
  Returns string like 'jdk-17' or 'jdk-11'."
  []
  (let [version (System/getProperty "java.version")
        ;; Extract major version (e.g., "17" from "17.0.1" or "11" from "11.0.12")
        major (first (str/split version #"\."))]
    (str "jdk-" major)))

;; ============================================================================
;; Library Source Location
;; ============================================================================

(defn find-sources-jar
  "Find -sources.jar for a given library JAR.

  ~/.m2/.../lib-1.0.jar -> ~/.m2/.../lib-1.0-sources.jar

  Returns path to sources JAR or nil if not found."
  [jar-path]
  (when jar-path
    (let [jar-str (str jar-path)
          ;; Replace .jar with -sources.jar
          sources-path (str/replace jar-str #"\.jar$" "-sources.jar")]
      (when (and (not= sources-path jar-str)
                 (fs/exists? sources-path))
        sources-path))))

;; ============================================================================
;; nREPL Class Location Resolution
;; ============================================================================

(def ^:private get-class-location-code
  "Clojure code to get the JAR location of a class via nREPL."
  "(fn [class-name-str]
     (try
       (let [cls (Class/forName class-name-str)
             resource (.getResource cls (str \"/\" (.replace class-name-str \".\" \"/\") \".class\"))
             url-str (when resource (str resource))]
         {:status \"found\"
          :class class-name-str
          :resource-url url-str})
       (catch ClassNotFoundException _
         {:status \"not-found\"
          :class class-name-str
          :reason \"Class not found\"})
       (catch Exception e
         {:status \"error\"
          :class class-name-str
          :reason (.getMessage e)})))")

(defn resolve-class-location
  "Resolve a Java class to its JAR location via nREPL.

  Returns map with:
  - :status - \"found\", \"not-found\", or \"error\"
  - :class - The class name
  - :resource-url - URL to the class file (e.g., jar:file:/...jar!/pkg/Class.class)
  - :reason - Error reason if not found"
  [conn class-name]
  (let [code (format "(%s %s)" get-class-location-code (pr-str class-name))
        response (nc/eval-nrepl* conn code)]
    (if-let [values (:value response)]
      (try
        (edn/read-string (first values))
        (catch Exception e
          {:status "error"
           :class class-name
           :reason (str "Failed to parse result: " (.getMessage e))}))
      {:status "error"
       :class class-name
       :reason (or (:err response) "No result from evaluation")})))

(defn extract-jar-from-resource-url
  "Extract JAR path from a resource URL.

  jar:file:/path/to/lib.jar!/com/example/Foo.class -> /path/to/lib.jar"
  [resource-url]
  (when resource-url
    (:jar-path (jar/parse-jar-uri resource-url))))

;; ============================================================================
;; Source Location Strategy
;; ============================================================================

(defn locate-java-source
  "Locate Java source for a class.

  For JDK classes: Uses src.zip from JAVA_HOME
  For library classes: Queries nREPL to find JAR, then looks for -sources.jar

  Returns map with:
  - :source-type - :jdk-src-zip, :sources-jar, :main-jar-source, or :needs-decompile
  - :source-path - Path to source archive (src.zip, sources.jar, or main JAR)
  - :entry-paths - Vector of paths to try within the archive (JDK 9+ has module prefixes)
  - :class-jar - Original class JAR (for decompilation fallback)
  - :error - Error message if source cannot be located"
  [conn class-name]
  (let [class-type (classify-java-class class-name)
        ;; For JDK classes, use module-prefixed paths; for libraries, just the simple path
        entry-paths (if (= :jdk class-type)
                      (class-to-source-paths class-name)
                      [(class-to-source-path class-name)])]
    (case class-type
      :jdk
      (if-let [src-zip (find-jdk-src-zip)]
        {:source-type :jdk-src-zip
         :source-path src-zip
         :entry-paths entry-paths
         :jdk-version (get-jdk-version)}
        {:source-type :error
         :error "JDK src.zip not found. Set JAVA_HOME or install JDK with sources."})

      :library
      (let [location (resolve-class-location conn class-name)]
        (if (= "found" (:status location))
          (let [class-jar (extract-jar-from-resource-url (:resource-url location))
                sources-jar (find-sources-jar class-jar)
                source-path (class-to-source-path class-name)]
            (cond
              ;; First choice: -sources.jar
              sources-jar
              {:source-type :sources-jar
               :source-path sources-jar
               :entry-paths entry-paths
               :class-jar class-jar}

              ;; Second choice: .java in main JAR (some libraries bundle sources)
              (jar/jar-contains-entry? class-jar source-path)
              {:source-type :main-jar-source
               :source-path class-jar
               :entry-paths entry-paths
               :class-jar class-jar}

              ;; Last resort: decompile
              :else
              {:source-type :needs-decompile
               :class-jar class-jar
               :class-path (class-to-class-path class-name)
               :entry-paths entry-paths}))
          {:source-type :error
           :error (or (:reason location) "Class not found")})))))

;; ============================================================================
;; Source Extraction
;; ============================================================================

(defn- try-extract-from-archive
  "Try to extract a file from an archive, attempting multiple entry paths.

  Parameters:
  - archive-path: Path to ZIP/JAR file
  - entry-paths: Vector of paths to try in order
  - dest-base: Base destination directory
  - extract-fn: Function to call (jar/extract-from-zip or jar/extract-jar-entry)

  Returns {:extracted path :entry-path entry} on success, nil on failure."
  [archive-path entry-paths dest-base extract-fn]
  (loop [paths entry-paths]
    (when (seq paths)
      (let [entry-path (first paths)
            dest-path (str (fs/path dest-base entry-path))]
        ;; Check if already cached
        (if (fs/exists? dest-path)
          {:extracted (str dest-path)
           :entry-path entry-path
           :cached? true}
          ;; Try to extract
          (if-let [extracted (extract-fn archive-path entry-path dest-path)]
            {:extracted extracted
             :entry-path entry-path
             :cached? false}
            ;; Try next path
            (recur (rest paths))))))))

(defn extract-java-source
  "Extract Java source file to cache directory.

  Parameters:
  - source-info: Map from locate-java-source
  - ctx: Session context

  Returns map with:
  - :status - \"found\" or \"error\"
  - :type - \"java-class\"
  - :class - The class name
  - :file - Local file path
  - :line - Always 1 for class definitions
  - :error - Error message if extraction failed"
  [source-info class-name ctx]
  (case (:source-type source-info)
    :jdk-src-zip
    (let [jdk-version (or (:jdk-version source-info) (get-jdk-version))
          dest-base (tmp/sources-dir ctx)
          dest-dir (str (fs/path dest-base jdk-version))
          entry-paths (:entry-paths source-info)
          result (try-extract-from-archive (:source-path source-info)
                                           entry-paths
                                           dest-dir
                                           jar/extract-from-zip)]
      (if result
        {:status "found"
         :type "java-class"
         :class class-name
         :file (:extracted result)
         :line 1
         :extraction-status (if (:cached? result) :cached :extracted)}
        {:status "error"
         :type "java-class"
         :class class-name
         :error (str "Failed to extract " (first entry-paths)
                     " from " (:source-path source-info)
                     " (tried " (count entry-paths) " paths)")}))

    :sources-jar
    (let [artifact (jar/infer-artifact-name (:source-path source-info))
          artifact-name (str artifact "-sources")
          dest-base (tmp/sources-dir ctx)
          dest-dir (str (fs/path dest-base artifact-name))
          entry-paths (:entry-paths source-info)
          result (try-extract-from-archive (:source-path source-info)
                                           entry-paths
                                           dest-dir
                                           jar/extract-jar-entry)]
      (if result
        {:status "found"
         :type "java-class"
         :class class-name
         :file (:extracted result)
         :line 1
         :extraction-status (if (:cached? result) :cached :extracted)}
        {:status "error"
         :type "java-class"
         :class class-name
         :error (str "Failed to extract " (first entry-paths)
                     " from " (:source-path source-info))}))

    :main-jar-source
    ;; Some libraries bundle .java sources directly in the main JAR
    (let [artifact (jar/infer-artifact-name (:source-path source-info))
          dest-base (tmp/sources-dir ctx)
          dest-dir (str (fs/path dest-base artifact))
          entry-paths (:entry-paths source-info)
          result (try-extract-from-archive (:source-path source-info)
                                           entry-paths
                                           dest-dir
                                           jar/extract-jar-entry)]
      (if result
        {:status "found"
         :type "java-class"
         :class class-name
         :file (:extracted result)
         :line 1
         :extraction-status (if (:cached? result) :cached :extracted)
         :bundled-source? true}
        {:status "error"
         :type "java-class"
         :class class-name
         :error (str "Failed to extract " (first entry-paths)
                     " from " (:source-path source-info))}))

    :needs-decompile
    (let [class-jar (:class-jar source-info)
          class-path (:class-path source-info)
          dest-dir (str (fs/path (tmp/sources-dir ctx)
                                 (str (jar/infer-artifact-name class-jar) "-decompiled")))
          result (decompiler/decompile-class-to-cache class-jar class-path class-name dest-dir)]
      (if (= :success (:status result))
        {:status "found"
         :type "java-class"
         :class class-name
         :file (:file result)
         :line 1
         :extraction-status (if (:cached? result) :cached :decompiled)
         :decompiled? true}
        {:status "error"
         :type "java-class"
         :class class-name
         :error (str (:error result)
                     " Tip: Try --extract-dep " class-jar " to check for bundled sources.")
         :class-jar class-jar}))

    :error
    {:status "error"
     :type "java-class"
     :class class-name
     :error (:error source-info)}

    ;; Default/unknown
    {:status "error"
     :type "java-class"
     :class class-name
     :error "Unknown source type"}))

;; ============================================================================
;; Main API
;; ============================================================================

(defn find-java-source
  "Find and extract source for a Java class.

  Parameters:
  - conn: nREPL connection (can be nil for JDK classes)
  - class-name: Fully qualified class name (e.g., \"java.util.HashMap\")
  - ctx: Session context map

  Returns map with:
  - :status - \"found\" or \"error\"
  - :type - \"java-class\"
  - :class - The class name
  - :file - Local file path (for Claude to read)
  - :line - Line number (1 for class definition)
  - :error - Error message if not found"
  [conn class-name ctx]
  (let [class-type (classify-java-class class-name)]
    ;; JDK classes don't need nREPL connection
    (if (and (= :library class-type) (nil? conn))
      {:status "error"
       :type "java-class"
       :class class-name
       :error "nREPL connection required to locate library class"}
      (let [source-info (locate-java-source conn class-name)]
        (extract-java-source source-info class-name ctx)))))
