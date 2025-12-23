(ns clojure-mcp-light.jar-extract-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-mcp-light.jar-extract :as jar]))

;; ============================================================================
;; JAR URI Parsing Tests
;; ============================================================================

(deftest parse-jar-uri-test
  (testing "parses standard jar:file: URI"
    (let [uri "jar:file:/home/user/.m2/repository/org/clojure/clojure/1.11.1/clojure-1.11.1.jar!/clojure/core.clj"
          result (jar/parse-jar-uri uri)]
      (is (= "/home/user/.m2/repository/org/clojure/clojure/1.11.1/clojure-1.11.1.jar"
             (:jar-path result)))
      (is (= "clojure/core.clj" (:entry-path result)))))

  (testing "parses jar:file: URI with triple slash"
    (let [uri "jar:file:///Users/me/.m2/repo/lib.jar!/com/example/Foo.clj"
          result (jar/parse-jar-uri uri)]
      (is (= "/Users/me/.m2/repo/lib.jar" (:jar-path result)))
      (is (= "com/example/Foo.clj" (:entry-path result)))))

  (testing "parses plain JAR path (no entry)"
    (let [path "/path/to/library.jar"
          result (jar/parse-jar-uri path)]
      (is (= "/path/to/library.jar" (:jar-path result)))
      (is (nil? (:entry-path result)))))

  (testing "returns nil for invalid input"
    (is (nil? (jar/parse-jar-uri nil)))
    (is (nil? (jar/parse-jar-uri "")))
    (is (nil? (jar/parse-jar-uri "/path/to/file.clj"))))

  (testing "accepts any path ending in .jar (even URLs)"
    ;; This is by design - we don't validate the path format
    (let [result (jar/parse-jar-uri "http://example.com/file.jar")]
      (is (= "http://example.com/file.jar" (:jar-path result)))
      (is (nil? (:entry-path result)))))

  (testing "handles nested directories in entry path"
    (let [uri "jar:file:/lib.jar!/com/example/nested/deeply/File.clj"
          result (jar/parse-jar-uri uri)]
      (is (= "/lib.jar" (:jar-path result)))
      (is (= "com/example/nested/deeply/File.clj" (:entry-path result))))))

;; ============================================================================
;; Artifact Name Inference Tests
;; ============================================================================

(deftest infer-artifact-name-test
  (testing "extracts artifact name from Maven repository path"
    (is (= "clojure-1.11.1"
           (jar/infer-artifact-name
            "~/.m2/repository/org/clojure/clojure/1.11.1/clojure-1.11.1.jar")))
    (is (= "ring-core-1.9.6"
           (jar/infer-artifact-name
            "~/.m2/repository/ring/ring-core/1.9.6/ring-core-1.9.6.jar"))))

  (testing "handles simple paths"
    (is (= "library" (jar/infer-artifact-name "/path/to/library.jar")))
    (is (= "foo" (jar/infer-artifact-name "foo.jar"))))

  (testing "handles paths without .jar extension"
    (is (= "file" (jar/infer-artifact-name "/path/to/file"))))

  (testing "returns nil for nil input"
    (is (nil? (jar/infer-artifact-name nil)))))

;; ============================================================================
;; JAR Extraction Tests (requires actual JAR file)
;; ============================================================================

(deftest extract-jar-entry-test
  (testing "returns nil for non-existent JAR"
    (is (nil? (jar/extract-jar-entry "/nonexistent.jar" "file.clj" "/tmp/dest.clj"))))

  (testing "returns nil for nil inputs"
    (is (nil? (jar/extract-jar-entry nil "file.clj" "/tmp/dest.clj")))
    (is (nil? (jar/extract-jar-entry "/path.jar" nil "/tmp/dest.clj")))
    (is (nil? (jar/extract-jar-entry "/path.jar" "file.clj" nil)))))

(deftest extract-jar-test
  (testing "returns error for non-existent JAR"
    (let [result (jar/extract-jar "/nonexistent.jar" "/tmp/dest")]
      (is (= :error (:status result)))
      (is (= "/nonexistent.jar" (:jar-path result)))
      (is (str/includes? (:error result) "does not exist")))))

(deftest list-jar-entries-test
  (testing "returns nil for non-existent JAR"
    (is (nil? (jar/list-jar-entries "/nonexistent.jar")))))

;; ============================================================================
;; Source Entry Detection Tests
;; ============================================================================

(deftest source-entry?-behavior-test
  ;; We test the behavior indirectly through list-source-entries
  (testing "list-source-entries returns nil for non-existent JAR"
    (is (nil? (jar/list-source-entries "/nonexistent.jar")))))
