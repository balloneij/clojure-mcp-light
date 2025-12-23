(ns clojure-mcp-light.source-nav-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-mcp-light.source-nav :as source-nav]
            [clojure-mcp-light.tmp :as tmp]
            [babashka.fs :as fs]))

;; ============================================================================
;; Symbol Parsing Tests
;; ============================================================================

(deftest parse-symbol-test
  (testing "parses qualified Clojure symbols"
    (let [result (source-nav/parse-symbol "clojure.core/map")]
      (is (= "clojure.core" (:ns result)))
      (is (= "map" (:name result)))
      (is (:qualified? result))
      (is (not (:java? result)))))

  (testing "parses qualified symbols with dots in namespace"
    (let [result (source-nav/parse-symbol "ring.util.response/redirect")]
      (is (= "ring.util.response" (:ns result)))
      (is (= "redirect" (:name result)))
      (is (:qualified? result))))

  (testing "parses Java class names"
    (let [result (source-nav/parse-symbol "java.util.HashMap")]
      (is (= "java.util.HashMap" (:class result)))
      (is (:java? result))))

  (testing "parses unqualified symbols"
    (let [result (source-nav/parse-symbol "map")]
      (is (nil? (:ns result)))
      (is (= "map" (:name result)))
      (is (not (:qualified? result)))
      (is (not (:java? result)))))

  (testing "returns nil for invalid input"
    (is (nil? (source-nav/parse-symbol nil)))
    (is (nil? (source-nav/parse-symbol "")))
    (is (nil? (source-nav/parse-symbol 123))))

  (testing "handles edge cases"
    ;; Symbol with just a slash (invalid)
    (let [result (source-nav/parse-symbol "/")]
      (is (nil? result)))
    ;; Trailing slash (should fail)
    (let [result (source-nav/parse-symbol "clojure.core/")]
      (is (nil? result)))
    ;; Leading slash (should fail)
    (let [result (source-nav/parse-symbol "/map")]
      (is (nil? result)))))

;; ============================================================================
;; Java Class Detection Tests
;; ============================================================================

(deftest java-class-symbol?-test
  (testing "detects Java class names"
    (is (source-nav/java-class-symbol? "java.util.HashMap"))
    (is (source-nav/java-class-symbol? "com.example.Foo"))
    (is (source-nav/java-class-symbol? "javax.swing.JFrame")))

  (testing "rejects Clojure symbols"
    (is (not (source-nav/java-class-symbol? "clojure.core/map")))
    (is (not (source-nav/java-class-symbol? "map")))
    (is (not (source-nav/java-class-symbol? "my-ns/my-fn"))))

  (testing "rejects invalid input"
    (is (not (source-nav/java-class-symbol? nil)))
    (is (not (source-nav/java-class-symbol? 123)))
    (is (not (source-nav/java-class-symbol? "")))))

;; ============================================================================
;; find-source Input Validation Tests
;; ============================================================================

(deftest find-source-validation-test
  (testing "handles JDK classes without nREPL connection"
    ;; JDK classes don't require nREPL - they come from src.zip
    (let [result (source-nav/find-source nil "java.util.HashMap")]
      ;; Should either succeed (if src.zip available) or fail with src.zip error
      (is (or (= "found" (:status result))
              (and (= "error" (:status result))
                   (not (str/includes? (or (:reason result) "") "nREPL")))))))

  (testing "library classes require nREPL connection"
    (let [result (source-nav/find-source nil "org.example.SomeClass")]
      (is (= "error" (:status result)))
      (is (str/includes? (:reason result) "nREPL"))))

  (testing "rejects invalid symbol format"
    (let [result (source-nav/find-source nil "")]
      (is (= "error" (:status result)))
      (is (str/includes? (:reason result) "Invalid"))))

  (testing "rejects unqualified symbol"
    (let [result (source-nav/find-source nil "map")]
      (is (= "error" (:status result)))
      (is (str/includes? (:reason result) "fully qualified")))))

;; ============================================================================
;; extract-dep Input Validation Tests
;; ============================================================================

(deftest extract-dep-validation-test
  (testing "returns error for non-existent JAR"
    (let [result (source-nav/extract-dep "/nonexistent/library.jar")]
      (is (= "error" (:status result)))
      (is (str/includes? (:error result) "does not exist"))))

  (testing "parses jar:file: URI and returns error for non-existent"
    (let [result (source-nav/extract-dep "jar:file:/nonexistent/library.jar!/some/file.clj")]
      (is (= "error" (:status result)))
      (is (str/includes? (:error result) "does not exist")))))

;; ============================================================================
;; tmp.clj Extension Tests
;; ============================================================================

(deftest sources-dir-test
  (testing "creates and returns sources directory"
    (let [ctx {:session-id "test-sources-session"
               :project-root "/test/sources/project"}
          sources-dir (tmp/sources-dir ctx)]
      (is (string? sources-dir))
      (is (str/ends-with? sources-dir "sources"))
      (is (fs/exists? sources-dir))
      (is (fs/directory? sources-dir))))

  (testing "is idempotent"
    (let [ctx {:session-id "test-sources-idempotent"
               :project-root "/test/sources/project2"}
          dir1 (tmp/sources-dir ctx)
          dir2 (tmp/sources-dir ctx)]
      (is (= dir1 dir2))
      (is (fs/exists? dir1)))))

(deftest source-cache-path-test
  (testing "generates correct cache path"
    (let [ctx {:session-id "test-cache-path"
               :project-root "/test/cache/project"}
          path (tmp/source-cache-path ctx "clojure-1.11.1" "clojure/core.clj")]
      (is (string? path))
      (is (str/includes? path "sources"))
      (is (str/includes? path "clojure-1.11.1"))
      (is (str/ends-with? path "clojure/core.clj"))))

  (testing "path is under sources directory"
    (let [ctx {:session-id "test-cache-under"
               :project-root "/test/cache/project2"}
          cache-path (tmp/source-cache-path ctx "lib-1.0" "com/example.clj")
          sources-dir (tmp/sources-dir ctx)]
      (is (str/starts-with? cache-path sources-dir)))))
