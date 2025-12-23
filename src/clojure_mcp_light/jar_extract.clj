(ns clojure-mcp-light.jar-extract
  "Extract files from JAR archives.

  Provides utilities for parsing JAR URIs, extracting individual entries
  or entire JARs to destination directories, and inferring artifact names
  from Maven repository paths."
  (:require [babashka.fs :as fs]
            [clojure.string :as str])
  (:import [java.util.jar JarFile]
           [java.util.zip ZipEntry]))

;; ============================================================================
;; JAR URI Parsing
;; ============================================================================

(defn parse-jar-uri
  "Parse jar:file:/path/to/lib.jar!/path/in/jar into components.

  Accepts formats:
  - jar:file:/path/to/lib.jar!/path/in/jar
  - jar:file:///path/to/lib.jar!/path/in/jar (triple slash)
  - /path/to/lib.jar (plain path, no entry)

  Returns:
  {:jar-path \"/path/to/lib.jar\"
   :entry-path \"path/in/jar\"}  ; or nil if no entry

  Returns nil if the input cannot be parsed."
  [uri-string]
  (when uri-string
    (cond
      ;; jar:file: URI with entry
      (str/starts-with? uri-string "jar:file:")
      (let [;; Remove jar:file: prefix
            without-prefix (subs uri-string 9)
            ;; Handle triple slash (jar:file:///path)
            normalized (if (str/starts-with? without-prefix "//")
                         (subs without-prefix 2)
                         without-prefix)
            ;; Split on !/ separator
            [jar-part entry-part] (str/split normalized #"!/" 2)]
        {:jar-path jar-part
         :entry-path entry-part})

      ;; Plain JAR path (no entry)
      (str/ends-with? uri-string ".jar")
      {:jar-path uri-string
       :entry-path nil}

      :else nil)))

;; ============================================================================
;; Artifact Name Inference
;; ============================================================================

(defn infer-artifact-name
  "Infer Maven artifact name from JAR path for cache organization.

  Examples:
  ~/.m2/repository/org/clojure/clojure/1.11.1/clojure-1.11.1.jar
  -> \"clojure-1.11.1\"

  ~/.m2/repository/ring/ring-core/1.9.6/ring-core-1.9.6.jar
  -> \"ring-core-1.9.6\"

  /some/random/path/foo.jar
  -> \"foo\"

  Returns the artifact name without the .jar extension."
  [jar-path]
  (when jar-path
    (let [filename (fs/file-name jar-path)]
      (if (str/ends-with? filename ".jar")
        (subs filename 0 (- (count filename) 4))
        filename))))

;; ============================================================================
;; JAR Entry Extraction
;; ============================================================================

(defn extract-jar-entry
  "Extract a single entry from a JAR to destination path.

  Parameters:
  - jar-path: Path to the JAR file
  - entry-path: Path within the JAR (e.g., \"clojure/core.clj\")
  - dest-path: Destination file path

  Returns dest-path on success, nil if entry not found.
  Creates parent directories as needed."
  [jar-path entry-path dest-path]
  (when (and jar-path entry-path dest-path (fs/exists? jar-path))
    (with-open [jar (JarFile. (str jar-path))]
      (when-let [entry (.getEntry jar entry-path)]
        (fs/create-dirs (fs/parent dest-path))
        (with-open [in (.getInputStream jar entry)]
          (fs/copy in dest-path {:replace-existing true}))
        (str dest-path)))))

(defn extract-jar-entries
  "Extract multiple entries from a JAR to destination directory.

  Parameters:
  - jar-path: Path to the JAR file
  - entry-paths: Collection of paths within the JAR
  - dest-dir: Destination directory

  Returns map of {entry-path -> dest-path} for successfully extracted files."
  [jar-path entry-paths dest-dir]
  (when (and jar-path (seq entry-paths) dest-dir (fs/exists? jar-path))
    (with-open [jar (JarFile. (str jar-path))]
      (reduce
       (fn [acc entry-path]
         (if-let [entry (.getEntry jar entry-path)]
           (let [dest-path (str (fs/path dest-dir entry-path))]
             (fs/create-dirs (fs/parent dest-path))
             (with-open [in (.getInputStream jar entry)]
               (fs/copy in dest-path {:replace-existing true}))
             (assoc acc entry-path dest-path))
           acc))
       {}
       entry-paths))))

;; ============================================================================
;; Full JAR Extraction
;; ============================================================================

(defn- source-entry?
  "Check if a JAR entry is a source file worth extracting.
  Includes .clj, .cljs, .cljc, .java, .edn files.
  Excludes META-INF, hidden files, and non-source content."
  [^ZipEntry entry]
  (let [name (.getName entry)]
    (and (not (.isDirectory entry))
         (not (str/starts-with? name "META-INF/"))
         (not (str/includes? name "/."))
         (or (str/ends-with? name ".clj")
             (str/ends-with? name ".cljs")
             (str/ends-with? name ".cljc")
             (str/ends-with? name ".java")
             (str/ends-with? name ".edn")))))

(defn extract-jar
  "Extract all source files from a JAR to destination directory.

  Parameters:
  - jar-path: Path to the JAR file
  - dest-dir: Destination directory

  Returns map with:
  - :status - :extracted or :error
  - :dir - Destination directory path
  - :jar-path - Source JAR path
  - :file-count - Number of files extracted
  - :files - List of extracted file paths (relative to dest-dir)
  - :error - Error message if status is :error"
  [jar-path dest-dir]
  (if-not (fs/exists? jar-path)
    {:status :error
     :jar-path (str jar-path)
     :error "JAR file does not exist"}
    (try
      (fs/create-dirs dest-dir)
      (with-open [jar (JarFile. (str jar-path))]
        (let [entries (enumeration-seq (.entries jar))
              source-entries (filter source-entry? entries)
              extracted (atom [])]
          (doseq [^ZipEntry entry source-entries]
            (let [entry-name (.getName entry)
                  dest-path (str (fs/path dest-dir entry-name))]
              (fs/create-dirs (fs/parent dest-path))
              (with-open [in (.getInputStream jar entry)]
                (fs/copy in dest-path {:replace-existing true}))
              (swap! extracted conj entry-name)))
          {:status :extracted
           :dir (str dest-dir)
           :jar-path (str jar-path)
           :file-count (count @extracted)
           :files @extracted}))
      (catch Exception e
        {:status :error
         :jar-path (str jar-path)
         :error (.getMessage e)}))))

;; ============================================================================
;; ZIP File Extraction (for JDK src.zip)
;; ============================================================================

(defn extract-from-zip
  "Extract a single entry from a ZIP file (e.g., JDK src.zip).

  Parameters:
  - zip-path: Path to the ZIP file
  - entry-path: Path within the ZIP (e.g., \"java/util/HashMap.java\")
  - dest-path: Destination file path

  Returns dest-path on success, nil if entry not found."
  [zip-path entry-path dest-path]
  (when (and zip-path entry-path dest-path (fs/exists? zip-path))
    (with-open [jar (JarFile. (str zip-path))]
      (when-let [entry (.getEntry jar entry-path)]
        (fs/create-dirs (fs/parent dest-path))
        (with-open [in (.getInputStream jar entry)]
          (fs/copy in dest-path {:replace-existing true}))
        (str dest-path)))))

;; ============================================================================
;; JAR Listing
;; ============================================================================

(defn list-jar-entries
  "List all entries in a JAR file.

  Parameters:
  - jar-path: Path to the JAR file
  - filter-fn: Optional predicate to filter entries (default: all entries)

  Returns vector of entry names (strings)."
  ([jar-path]
   (list-jar-entries jar-path (constantly true)))
  ([jar-path filter-fn]
   (when (fs/exists? jar-path)
     (with-open [jar (JarFile. (str jar-path))]
       (->> (enumeration-seq (.entries jar))
            (filter filter-fn)
            (mapv #(.getName ^ZipEntry %)))))))

(defn list-source-entries
  "List all source file entries in a JAR.
  Convenience wrapper around list-jar-entries."
  [jar-path]
  (list-jar-entries jar-path source-entry?))
