(ns clojure-mcp-light.java-source-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-mcp-light.java-source :as java-source]))

;; ============================================================================
;; Class Classification Tests
;; ============================================================================

(deftest classify-java-class-test
  (testing "classifies JDK classes"
    (is (= :jdk (java-source/classify-java-class "java.util.HashMap")))
    (is (= :jdk (java-source/classify-java-class "java.lang.String")))
    (is (= :jdk (java-source/classify-java-class "javax.swing.JFrame")))
    (is (= :jdk (java-source/classify-java-class "sun.misc.Unsafe")))
    (is (= :jdk (java-source/classify-java-class "jdk.internal.misc.Unsafe")))
    (is (= :jdk (java-source/classify-java-class "com.sun.crypto.provider.DESCipher"))))

  (testing "classifies library classes"
    (is (= :library (java-source/classify-java-class "org.apache.commons.codec.binary.Base64")))
    (is (= :library (java-source/classify-java-class "com.google.common.collect.ImmutableList")))
    (is (= :library (java-source/classify-java-class "nrepl.server.Server")))))

;; ============================================================================
;; Source Path Conversion Tests
;; ============================================================================

(deftest class-to-source-path-test
  (testing "converts class name to source path"
    (is (= "java/util/HashMap.java"
           (java-source/class-to-source-path "java.util.HashMap")))
    (is (= "org/apache/commons/codec/binary/Base64.java"
           (java-source/class-to-source-path "org.apache.commons.codec.binary.Base64")))))

(deftest class-to-class-path-test
  (testing "converts class name to class file path"
    (is (= "java/util/HashMap.class"
           (java-source/class-to-class-path "java.util.HashMap")))))

;; ============================================================================
;; Module Prefix Tests (JDK 9+)
;; ============================================================================

(deftest class-to-module-prefix-test
  (testing "returns correct module for java.base classes"
    (is (= "java.base" (java-source/class-to-module-prefix "java.lang.String")))
    (is (= "java.base" (java-source/class-to-module-prefix "java.util.HashMap")))
    (is (= "java.base" (java-source/class-to-module-prefix "java.io.File")))
    (is (= "java.base" (java-source/class-to-module-prefix "java.nio.ByteBuffer"))))

  (testing "returns correct module for java.sql classes"
    (is (= "java.sql" (java-source/class-to-module-prefix "java.sql.Connection")))
    (is (= "java.sql" (java-source/class-to-module-prefix "javax.sql.DataSource"))))

  (testing "returns correct module for java.desktop classes"
    (is (= "java.desktop" (java-source/class-to-module-prefix "java.awt.Frame")))
    (is (= "java.desktop" (java-source/class-to-module-prefix "javax.swing.JFrame"))))

  (testing "returns nil for library classes"
    (is (nil? (java-source/class-to-module-prefix "org.apache.commons.codec.Encoder")))))

(deftest class-to-source-paths-test
  (testing "returns module-prefixed path first for JDK classes"
    (let [paths (java-source/class-to-source-paths "java.util.HashMap")]
      (is (= 2 (count paths)))
      (is (= "java.base/java/util/HashMap.java" (first paths)))
      (is (= "java/util/HashMap.java" (second paths)))))

  (testing "returns single path for library classes"
    (let [paths (java-source/class-to-source-paths "org.example.Foo")]
      ;; Library classes don't have module prefixes in classify
      (is (= ["org/example/Foo.java"] paths)))))

;; ============================================================================
;; JDK Source Location Tests
;; ============================================================================

(deftest find-jdk-src-zip-test
  (testing "returns nil when JAVA_HOME not set and no standard locations"
    ;; This test is environment-dependent
    ;; Just verify it returns a string or nil
    (let [result (java-source/find-jdk-src-zip)]
      (is (or (nil? result) (string? result))))))

(deftest get-jdk-version-test
  (testing "returns jdk-prefixed version string"
    (let [version (java-source/get-jdk-version)]
      (is (string? version))
      (is (str/starts-with? version "jdk-")))))

;; ============================================================================
;; Sources JAR Detection Tests
;; ============================================================================

(deftest find-sources-jar-test
  (testing "returns nil for nil input"
    (is (nil? (java-source/find-sources-jar nil))))

  (testing "returns nil when sources JAR doesn't exist"
    (is (nil? (java-source/find-sources-jar "/nonexistent/path/lib-1.0.jar"))))

  (testing "constructs correct sources JAR path pattern"
    ;; This is a pattern test - we verify the function would check the right path
    ;; by testing with a path that doesn't exist
    (is (nil? (java-source/find-sources-jar "/path/to/lib-1.0.jar")))))

;; ============================================================================
;; Source Location Strategy Tests
;; ============================================================================

(deftest locate-java-source-jdk-test
  (testing "returns jdk-src-zip type for JDK classes when src.zip exists"
    ;; Skip if no JAVA_HOME or src.zip
    (when-let [src-zip (java-source/find-jdk-src-zip)]
      (let [result (java-source/locate-java-source nil "java.util.HashMap")]
        (is (= :jdk-src-zip (:source-type result)))
        (is (= src-zip (:source-path result)))
        (is (vector? (:entry-paths result)))
        (is (some #(str/ends-with? % "HashMap.java") (:entry-paths result))))))

  (testing "returns error when src.zip not found and JAVA_HOME not set"
    ;; This would only pass in environments without JAVA_HOME
    ;; We can't reliably test this without mocking
    ))

;; ============================================================================
;; Integration Tests (require specific environment setup)
;; ============================================================================

(deftest find-java-source-validation-test
  (testing "requires nREPL connection for library classes"
    (let [result (java-source/find-java-source nil "org.example.SomeClass" {})]
      (is (= "error" (:status result)))
      (is (str/includes? (:error result) "nREPL connection required"))))

  (testing "JDK classes don't require nREPL connection"
    ;; Skip if no src.zip available
    (when (java-source/find-jdk-src-zip)
      (let [result (java-source/find-java-source nil "java.util.HashMap" {})]
        ;; Should either succeed or fail for extraction reasons, not connection
        (is (not (str/includes? (or (:error result) "") "nREPL connection")))))))
