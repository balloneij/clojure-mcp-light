(ns clojure-mcp-light.decompiler
  "Java class file decompilation support.

  Supports decompiling .class files to .java source using external decompilers.
  Currently supports CFR (https://github.com/leibnitz27/cfr).

  Setup:
  1. Download CFR JAR from https://github.com/leibnitz27/cfr/releases
  2. Either:
     a. Set CFR_JAR environment variable to the JAR path
     b. Place 'cfr' wrapper script on PATH that runs the JAR

  Example wrapper script (save as 'cfr' on PATH):
    #!/bin/bash
    java -jar /path/to/cfr.jar \"$@\""
  (:require [babashka.fs :as fs]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

;; ============================================================================
;; Decompiler Detection
;; ============================================================================

(defn find-cfr-jar
  "Find CFR decompiler JAR.

  Checks in order:
  1. CFR_JAR environment variable
  2. Common locations in user home

  Returns path to CFR JAR or nil if not found."
  []
  (let [env-jar (System/getenv "CFR_JAR")
        home (System/getProperty "user.home")
        common-locations [(str home "/.local/lib/cfr.jar")
                          (str home "/lib/cfr.jar")
                          (str home "/.cfr/cfr.jar")]]
    (or (when (and env-jar (fs/exists? env-jar)) env-jar)
        (first (filter fs/exists? common-locations)))))

(defn cfr-command-available?
  "Check if 'cfr' command is available on PATH."
  []
  (try
    (let [result (sh "which" "cfr")]
      (zero? (:exit result)))
    (catch Exception _
      false)))

(defn decompiler-available?
  "Check if any decompiler is available.

  Returns map with:
  - :available? - boolean
  - :type - :cfr-jar, :cfr-command, or nil
  - :path - path to JAR or command"
  []
  (cond
    (find-cfr-jar)
    {:available? true
     :type :cfr-jar
     :path (find-cfr-jar)}

    (cfr-command-available?)
    {:available? true
     :type :cfr-command
     :path "cfr"}

    :else
    {:available? false
     :type nil
     :path nil}))

;; ============================================================================
;; Class File Extraction
;; ============================================================================

(defn extract-class-from-jar
  "Extract a .class file from a JAR to a temporary location.

  Parameters:
  - jar-path: Path to the JAR file
  - class-path: Path within the JAR (e.g., \"com/example/Foo.class\")
  - dest-dir: Directory to extract to

  Returns path to extracted .class file or nil on failure."
  [jar-path class-path dest-dir]
  (when (and jar-path class-path dest-dir (fs/exists? jar-path))
    (let [dest-path (str (fs/path dest-dir class-path))]
      (fs/create-dirs (fs/parent dest-path))
      (try
        (with-open [jar (java.util.jar.JarFile. (str jar-path))]
          (when-let [entry (.getEntry jar class-path)]
            (with-open [in (.getInputStream jar entry)]
              (fs/copy in dest-path {:replace-existing true}))
            (str dest-path)))
        (catch Exception _
          nil)))))

;; ============================================================================
;; CFR Decompilation
;; ============================================================================

(defn- run-cfr-jar
  "Run CFR decompiler using JAR file.

  Returns {:exit code :out stdout :err stderr}"
  [cfr-jar class-file output-dir]
  (sh "java" "-jar" cfr-jar
      (str class-file)
      "--outputdir" (str output-dir)
      "--silent" "true"))

(defn- run-cfr-command
  "Run CFR decompiler using command on PATH.

  Returns {:exit code :out stdout :err stderr}"
  [class-file output-dir]
  (sh "cfr"
      (str class-file)
      "--outputdir" (str output-dir)
      "--silent" "true"))

(defn decompile-with-cfr
  "Decompile a .class file using CFR.

  Parameters:
  - class-file: Path to .class file
  - output-dir: Directory for output .java file
  - decompiler-info: Map from decompiler-available? (optional)

  Returns map with:
  - :status - :success or :error
  - :output-file - Path to decompiled .java file (if successful)
  - :error - Error message (if failed)"
  ([class-file output-dir]
   (decompile-with-cfr class-file output-dir (decompiler-available?)))
  ([class-file output-dir decompiler-info]
   (if-not (:available? decompiler-info)
     {:status :error
      :error "No decompiler available. Install CFR and set CFR_JAR or add 'cfr' to PATH."}
     (try
       (fs/create-dirs output-dir)
       (let [result (case (:type decompiler-info)
                      :cfr-jar (run-cfr-jar (:path decompiler-info) class-file output-dir)
                      :cfr-command (run-cfr-command class-file output-dir))]
         (if (zero? (:exit result))
           ;; Find the generated .java file
           (let [class-name (-> (str class-file)
                                (fs/file-name)
                                (str/replace #"\.class$" ""))
                 ;; CFR creates package directories, find the .java file
                 java-files (->> (fs/glob output-dir "**/*.java")
                                 (filter #(= (str (fs/file-name %))
                                             (str class-name ".java"))))]
             (if (seq java-files)
               {:status :success
                :output-file (str (first java-files))}
               {:status :error
                :error (str "Decompilation succeeded but output file not found for " class-name)}))
           {:status :error
            :error (or (not-empty (:err result))
                       (str "CFR exited with code " (:exit result)))}))
       (catch Exception e
         {:status :error
          :error (str "Decompilation failed: " (.getMessage e))})))))

;; ============================================================================
;; Header Comment
;; ============================================================================

(defn add-decompiled-header
  "Add a header comment to decompiled source indicating it was decompiled.

  Parameters:
  - java-file: Path to .java file
  - class-name: Original class name
  - jar-path: Path to original JAR (optional)

  Returns path to modified file."
  [java-file class-name jar-path]
  (when (fs/exists? java-file)
    (let [content (slurp java-file)
          header (str "/*\n"
                      " * Decompiled by clj-nrepl-eval\n"
                      " * Class: " class-name "\n"
                      (when jar-path
                        (str " * JAR: " jar-path "\n"))
                      " *\n"
                      " * Note: Line numbers may not match original source.\n"
                      " */\n\n")
          ;; Insert header after package declaration if present
          new-content (if (str/starts-with? content "package ")
                        (let [pkg-end (str/index-of content "\n")]
                          (str (subs content 0 (inc pkg-end))
                               "\n" header
                               (subs content (inc pkg-end))))
                        (str header content))]
      (spit java-file new-content)
      (str java-file))))

;; ============================================================================
;; Main API
;; ============================================================================

(defn decompile-class
  "Decompile a Java class from a JAR file.

  Parameters:
  - jar-path: Path to JAR containing the class
  - class-path: Path within JAR (e.g., \"com/example/Foo.class\")
  - class-name: Fully qualified class name (e.g., \"com.example.Foo\")
  - output-dir: Directory for decompiled output

  Returns map with:
  - :status - :success or :error
  - :file - Path to decompiled .java file (if successful)
  - :error - Error message (if failed)"
  [jar-path class-path class-name output-dir]
  (let [decompiler-info (decompiler-available?)]
    (if-not (:available? decompiler-info)
      {:status :error
       :error "No decompiler available. Install CFR: download from https://github.com/leibnitz27/cfr/releases and set CFR_JAR environment variable."}
      (let [;; Create temp directory for class extraction
            temp-dir (str (fs/create-temp-dir {:prefix "cfr-"}))]
        (try
          ;; Extract class file from JAR
          (if-let [class-file (extract-class-from-jar jar-path class-path temp-dir)]
            ;; Decompile
            (let [result (decompile-with-cfr class-file output-dir decompiler-info)]
              (if (= :success (:status result))
                ;; Add header comment
                (do
                  (add-decompiled-header (:output-file result) class-name jar-path)
                  {:status :success
                   :file (:output-file result)})
                result))
            {:status :error
             :error (str "Failed to extract " class-path " from " jar-path)})
          (finally
            ;; Cleanup temp directory
            (fs/delete-tree temp-dir)))))))

(defn decompile-class-to-cache
  "Decompile a Java class to the session source cache.

  This is the main entry point for decompilation, integrating with
  the source navigation cache structure.

  Parameters:
  - jar-path: Path to JAR containing the class
  - class-path: Path within JAR (e.g., \"com/example/Foo.class\")
  - class-name: Fully qualified class name
  - dest-dir: Cache directory for this artifact

  Returns map with:
  - :status - :success or :error
  - :file - Path to decompiled .java file
  - :error - Error message if failed"
  [jar-path class-path class-name dest-dir]
  ;; Check if already decompiled (CFR outputs with package structure)
  (let [java-file-pattern (str (str/replace class-name "." "/") ".java")
        existing-file (first (fs/glob dest-dir (str "**/" (fs/file-name java-file-pattern))))]
    (if (and existing-file (fs/exists? existing-file))
      {:status :success
       :file (str existing-file)
       :cached? true}
      (let [result (decompile-class jar-path class-path class-name dest-dir)]
        (if (= :success (:status result))
          (assoc result :cached? false)
          result)))))
