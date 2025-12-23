(ns clojure-mcp-light.source-nav
  "Resolve symbols to source locations via nREPL.

  Provides functionality to locate and extract source files for Clojure vars
  and Java classes from third-party libraries by querying metadata from a
  running REPL."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure-mcp-light.jar-extract :as jar]
            [clojure-mcp-light.java-source :as java-source]
            [clojure-mcp-light.nrepl-client :as nc]
            [clojure-mcp-light.tmp :as tmp]))

;; ============================================================================
;; Symbol Parsing
;; ============================================================================

(defn parse-symbol
  "Parse a qualified symbol string into namespace and name parts.

  Examples:
  \"clojure.core/map\" -> {:ns \"clojure.core\" :name \"map\" :qualified? true}
  \"map\" -> {:ns nil :name \"map\" :qualified? false}
  \"java.util.HashMap\" -> {:class \"java.util.HashMap\" :java? true}

  Returns nil for invalid input."
  [sym-str]
  (when (and sym-str (string? sym-str) (seq sym-str))
    (cond
      ;; Qualified Clojure symbol (ns/name)
      (str/includes? sym-str "/")
      (let [[ns-part name-part] (str/split sym-str #"/" 2)]
        (when (and (seq ns-part) (seq name-part))
          {:ns ns-part
           :name name-part
           :qualified? true
           :java? false}))

      ;; Java class (contains dots, no slash)
      (str/includes? sym-str ".")
      {:class sym-str
       :java? true}

      ;; Unqualified symbol
      :else
      {:ns nil
       :name sym-str
       :qualified? false
       :java? false})))

(defn java-class-symbol?
  "Check if a symbol string represents a Java class.
  Returns true for patterns like \"java.util.HashMap\" or \"com.example.Foo\"."
  [sym-str]
  (and (string? sym-str)
       (str/includes? sym-str ".")
       (not (str/includes? sym-str "/"))))

;; ============================================================================
;; nREPL Symbol Resolution
;; ============================================================================

(def ^:private get-var-source-info-code
  "Clojure code to evaluate on the REPL to get var source info.
  Takes a qualified symbol and returns a map with metadata."
  "(fn [qualified-sym-str]
     (try
       (let [sym (symbol qualified-sym-str)]
         (if-let [v (resolve sym)]
           (let [m (meta v)
                 file (:file m)
                 jar-uri (when file
                           (try
                             (some-> (clojure.java.io/resource file) str)
                             (catch Exception _ nil)))]
             {:status \"found\"
              :type \"clojure-var\"
              :symbol qualified-sym-str
              :namespace (some-> (:ns m) str)
              :name (some-> (:name m) str)
              :file file
              :line (:line m)
              :column (:column m)
              :jar-uri jar-uri
              :arglists (when (:arglists m) (str (:arglists m)))
              :doc (:doc m)})
           {:status \"not-found\"
            :symbol qualified-sym-str
            :reason \"Symbol could not be resolved\"}))
       (catch Exception e
         {:status \"error\"
          :symbol qualified-sym-str
          :reason (.getMessage e)})))")

(defn resolve-clojure-source
  "Resolve a Clojure symbol to its source location via nREPL.

  Parameters:
  - conn: nREPL connection map with :input, :output, :host, :port, :session-id
  - qualified-symbol: String like \"clojure.core/map\"

  Returns map with:
  - :status - \"found\", \"not-found\", or \"error\"
  - :type - \"clojure-var\" if found
  - :symbol - The requested symbol
  - :namespace - Namespace string
  - :name - Var name
  - :file - Classpath-relative file path (e.g., \"clojure/core.clj\")
  - :line - Line number
  - :column - Column number
  - :jar-uri - Full JAR URI (e.g., \"jar:file:/.../.m2/.../clojure.jar!/clojure/core.clj\")
  - :arglists - Function arglists as string
  - :doc - Docstring
  - :reason - Error reason if not found"
  [conn qualified-symbol]
  (let [;; Build evaluation code - wrap fn in parens to call it
        code (format "(%s %s)" get-var-source-info-code (pr-str qualified-symbol))
        ;; Evaluate via nREPL
        response (nc/eval-nrepl* conn code)]
    (if-let [error (:error response)]
      {:status "error"
       :symbol qualified-symbol
       :reason error}
      (if-let [values (:value response)]
        ;; Parse the first value (should be a map printed as EDN)
        (try
          (let [result-str (first values)
                result (edn/read-string result-str)]
            result)
          (catch Exception e
            {:status "error"
             :symbol qualified-symbol
             :reason (str "Failed to parse result: " (.getMessage e))}))
        {:status "error"
         :symbol qualified-symbol
         :reason "No result from evaluation"}))))

;; ============================================================================
;; Source File Extraction
;; ============================================================================

(defn extract-source-file
  "Extract a source file from a JAR to the session source cache.

  Parameters:
  - ctx: Session context map
  - source-info: Map from resolve-clojure-source with :jar-uri and :file

  Returns updated source-info map with:
  - :local-file - Path to extracted file on disk
  - :extraction-status - :extracted, :cached, or :error
  - :error - Error message if extraction failed"
  [ctx source-info]
  (if (not= "found" (:status source-info))
    source-info
    (let [{:keys [jar-uri file]} source-info]
      (if-not jar-uri
        (assoc source-info
               :extraction-status :error
               :error "No JAR URI available - source may be local or unavailable")
        (let [parsed (jar/parse-jar-uri jar-uri)]
          (if-not parsed
            (assoc source-info
                   :extraction-status :error
                   :error (str "Failed to parse JAR URI: " jar-uri))
            (let [{:keys [jar-path entry-path]} parsed
                  artifact (jar/infer-artifact-name jar-path)
                  dest-path (tmp/source-cache-path ctx artifact file)]
              ;; Check if already cached
              (if (fs/exists? dest-path)
                (assoc source-info
                       :local-file (str dest-path)
                       :extraction-status :cached)
                ;; Extract from JAR
                (if-let [extracted (jar/extract-jar-entry jar-path entry-path dest-path)]
                  (assoc source-info
                         :local-file extracted
                         :extraction-status :extracted)
                  (assoc source-info
                         :extraction-status :error
                         :error (str "Failed to extract " entry-path " from " jar-path)))))))))))

;; ============================================================================
;; Main API
;; ============================================================================

(defn find-source
  "Find and extract source for a symbol.

  This is the main entry point that:
  1. Resolves the symbol via nREPL
  2. Extracts the source file from its JAR
  3. Returns the local file path

  Parameters:
  - conn: nREPL connection map
  - symbol-str: Qualified symbol string (e.g., \"clojure.core/map\")
  - ctx: Session context map (optional, defaults to {})

  Returns JSON-friendly map with:
  - :status - \"found\", \"not-found\", or \"error\"
  - :type - \"clojure-var\" if found
  - :symbol - The requested symbol
  - :file - Local file path (key output for Claude)
  - :line - Line number
  - :column - Column number
  - :arglists - Function arglists
  - :doc - Docstring (truncated if very long)
  - :error - Error message if not found"
  ([conn symbol-str]
   (find-source conn symbol-str {}))
  ([conn symbol-str ctx]
   (let [parsed (parse-symbol symbol-str)]
     (cond
       (nil? parsed)
       {:status "error"
        :symbol symbol-str
        :reason "Invalid symbol format"}

       (:java? parsed)
       ;; Java class handling
       (let [result (java-source/find-java-source conn (:class parsed) ctx)]
         ;; Normalize response format to match Clojure var output
         (cond-> {:status (:status result)
                  :type (:type result)
                  :symbol symbol-str}
           (= "found" (:status result))
           (merge {:file (:file result)
                   :line (:line result)})
           (not= "found" (:status result))
           (assoc :reason (:error result))))

       (not (:qualified? parsed))
       {:status "error"
        :symbol symbol-str
        :reason "Symbol must be fully qualified (e.g., clojure.core/map)"}

       :else
       (let [source-info (resolve-clojure-source conn symbol-str)
             extracted (extract-source-file ctx source-info)]
         ;; Build final result - rename :local-file to :file for clarity
         (cond-> {:status (:status extracted)
                  :type (:type extracted)
                  :symbol (:symbol extracted)}

           (= "found" (:status extracted))
           (merge {:file (or (:local-file extracted) (:file extracted))
                   :line (:line extracted)
                   :column (:column extracted)
                   :arglists (:arglists extracted)
                   :doc (when-let [doc (:doc extracted)]
                          ;; Truncate very long docstrings
                          (if (> (count doc) 500)
                            (str (subs doc 0 497) "...")
                            doc))})

           (not= "found" (:status extracted))
           (assoc :reason (or (:error extracted) (:reason extracted)))))))))

;; ============================================================================
;; Dependency Extraction
;; ============================================================================

(defn extract-dep
  "Extract an entire JAR/dependency to a directory.

  This allows Claude to explore an entire library using Glob, Grep, and Read.

  Parameters:
  - jar-path-or-uri: Either a JAR path or a jar:file: URI
  - ctx: Session context map (optional, defaults to {})

  Returns JSON-friendly map with:
  - :status - \"extracted\" or \"error\"
  - :dir - Directory containing extracted files
  - :source-jar - Path to the source JAR
  - :file-count - Number of files extracted
  - :error - Error message if failed"
  ([jar-path-or-uri]
   (extract-dep jar-path-or-uri {}))
  ([jar-path-or-uri ctx]
   (let [;; Parse if it's a jar:file: URI, otherwise use as-is
         parsed (jar/parse-jar-uri jar-path-or-uri)
         jar-path (if parsed (:jar-path parsed) jar-path-or-uri)]
     (if-not (fs/exists? jar-path)
       {:status "error"
        :jar-path jar-path
        :error "JAR file does not exist"}
       (let [artifact (jar/infer-artifact-name jar-path)
             dest-dir (str (fs/path (tmp/sources-dir ctx) artifact))
             result (jar/extract-jar jar-path dest-dir)]
         ;; Transform to JSON-friendly format
         (if (= :extracted (:status result))
           {:status "extracted"
            :dir (:dir result)
            :source-jar (:jar-path result)
            :file-count (:file-count result)}
           {:status "error"
            :jar-path (str jar-path)
            :error (or (:error result) "Unknown extraction error")}))))))
