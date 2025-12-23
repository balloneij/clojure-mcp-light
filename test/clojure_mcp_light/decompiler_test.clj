(ns clojure-mcp-light.decompiler-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-mcp-light.decompiler :as decompiler]
            [babashka.fs :as fs]))

;; ============================================================================
;; Decompiler Detection Tests
;; ============================================================================

(deftest find-cfr-jar-test
  (testing "returns nil when no CFR JAR is found"
    ;; This is environment-dependent - just verify return type
    (let [result (decompiler/find-cfr-jar)]
      (is (or (nil? result) (string? result)))))

  (testing "respects CFR_JAR environment variable"
    ;; If CFR_JAR is set and exists, it should be returned
    (when-let [env-jar (System/getenv "CFR_JAR")]
      (when (fs/exists? env-jar)
        (is (= env-jar (decompiler/find-cfr-jar)))))))

(deftest cfr-command-available?-test
  (testing "returns boolean"
    (is (boolean? (decompiler/cfr-command-available?)))))

(deftest decompiler-available?-test
  (testing "returns map with required keys"
    (let [result (decompiler/decompiler-available?)]
      (is (map? result))
      (is (contains? result :available?))
      (is (contains? result :type))
      (is (contains? result :path))
      (is (boolean? (:available? result)))))

  (testing "type is valid when available"
    (let [result (decompiler/decompiler-available?)]
      (when (:available? result)
        (is (#{:cfr-jar :cfr-command} (:type result)))
        (is (string? (:path result))))))

  (testing "type is nil when not available"
    (let [result (decompiler/decompiler-available?)]
      (when-not (:available? result)
        (is (nil? (:type result)))))))

;; ============================================================================
;; Class File Extraction Tests
;; ============================================================================

(deftest extract-class-from-jar-test
  (testing "returns nil for non-existent JAR"
    (is (nil? (decompiler/extract-class-from-jar
               "/nonexistent.jar"
               "com/example/Foo.class"
               "/tmp/test"))))

  (testing "returns nil for nil inputs"
    (is (nil? (decompiler/extract-class-from-jar nil "foo.class" "/tmp")))
    (is (nil? (decompiler/extract-class-from-jar "/path.jar" nil "/tmp")))
    (is (nil? (decompiler/extract-class-from-jar "/path.jar" "foo.class" nil)))))

;; ============================================================================
;; Decompilation Result Tests
;; ============================================================================

(deftest decompile-with-cfr-test
  (testing "returns error when no decompiler available"
    (let [result (decompiler/decompile-with-cfr
                  "/nonexistent/Foo.class"
                  "/tmp/output"
                  {:available? false :type nil :path nil})]
      (is (= :error (:status result)))
      (is (str/includes? (:error result) "No decompiler available")))))

(deftest decompile-class-test
  (testing "returns error for non-existent JAR"
    (let [result (decompiler/decompile-class
                  "/nonexistent.jar"
                  "com/example/Foo.class"
                  "com.example.Foo"
                  "/tmp/output")]
      (is (= :error (:status result)))
      (is (or (str/includes? (:error result) "No decompiler")
              (str/includes? (:error result) "Failed to extract"))))))

(deftest decompile-class-to-cache-test
  (testing "returns error for non-existent JAR"
    (let [result (decompiler/decompile-class-to-cache
                  "/nonexistent.jar"
                  "com/example/Foo.class"
                  "com.example.Foo"
                  "/tmp/cache")]
      (is (= :error (:status result))))))

;; ============================================================================
;; Header Comment Tests
;; ============================================================================

(deftest add-decompiled-header-test
  (testing "returns nil for non-existent file"
    (is (nil? (decompiler/add-decompiled-header
               "/nonexistent/Foo.java"
               "com.example.Foo"
               "/path/to.jar"))))

  (testing "adds header to file"
    (let [temp-dir (str (fs/create-temp-dir {:prefix "decompiler-test-"}))
          java-file (str (fs/path temp-dir "Test.java"))
          original-content "public class Test {\n    // code\n}"]
      (try
        ;; Create test file
        (spit java-file original-content)

        ;; Add header
        (decompiler/add-decompiled-header java-file "com.example.Test" "/path/to/lib.jar")

        ;; Verify header was added
        (let [new-content (slurp java-file)]
          (is (str/includes? new-content "Decompiled by clj-nrepl-eval"))
          (is (str/includes? new-content "com.example.Test"))
          (is (str/includes? new-content "/path/to/lib.jar"))
          (is (str/includes? new-content original-content)))
        (finally
          (fs/delete-tree temp-dir)))))

  (testing "inserts header after package declaration"
    (let [temp-dir (str (fs/create-temp-dir {:prefix "decompiler-test-"}))
          java-file (str (fs/path temp-dir "Test.java"))
          original-content "package com.example;\n\npublic class Test {}"]
      (try
        (spit java-file original-content)
        (decompiler/add-decompiled-header java-file "com.example.Test" nil)
        (let [new-content (slurp java-file)]
          ;; Package should still be at the start
          (is (str/starts-with? new-content "package com.example;"))
          ;; Header should come after package
          (is (str/includes? new-content "Decompiled by")))
        (finally
          (fs/delete-tree temp-dir))))))
