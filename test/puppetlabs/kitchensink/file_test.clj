(ns puppetlabs.kitchensink.file-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [puppetlabs.kitchensink.file :refer [atomic-write get-perms set-perms unzip-file]])
  (:import (java.io BufferedWriter File FileNotFoundException)))

(deftest atomic-write-test
  (testing "when the file doesn't exist"
    (let [tmp-file (.toString ^File (fs/temp-file "atomic-writes-test"))
          content "Something said, not good"]
      (fs/delete tmp-file)
      (atomic-write tmp-file #(.write ^BufferedWriter % content))

      (testing "it writes the data"
        (is (= content (slurp tmp-file))))

      (testing "it applies a default mode of 0640"
        (is (= "rw-r-----" (get-perms tmp-file))))

      (testing "it applies the specified mode"
        (fs/delete tmp-file)
        (atomic-write tmp-file #(.write ^BufferedWriter % content) "rwxrwxrwx")
        (is (= "rwxrwxrwx" (get-perms tmp-file))))

      (testing "it can create a read-only file"
        (fs/delete tmp-file)
        (atomic-write tmp-file #(.write ^BufferedWriter % content) "r--------")
        (is (= "r--------" (get-perms tmp-file))))

      (fs/delete tmp-file)))

  (testing "when overwriting"
    (let [tmp-file (.toString ^File (fs/temp-file "atomic-writes-test"))
          content "Something said, not good"]
      (set-perms tmp-file "rwxr-x--x")

      (testing "it preserves existing permissions"
        (atomic-write tmp-file #(.write ^BufferedWriter % content))
        (is (= "rwxr-x--x" (get-perms tmp-file))))

      (testing "it overwrites existing permissions if specified"
        (atomic-write tmp-file #(.write ^BufferedWriter % content) "rw-rw----")
        (is (= "rw-rw----" (get-perms tmp-file))))

      (testing "it updates a read-only file"
        (set-perms tmp-file "r--------")
        (atomic-write tmp-file #(.write ^BufferedWriter % content))
        (is (= content (slurp tmp-file))))

      (fs/delete tmp-file)))

  (testing "the supplied function is allowed to close the writer"
    (let [tmp-file (.toString ^File (fs/temp-file "atomic-writes-test"))
          write-fn (fn [^BufferedWriter writer]
                     (.write writer "They called me slow")
                     (.close writer))]

      (atomic-write tmp-file write-fn)

      (fs/delete tmp-file))))

(deftest unzip-file-test
  (testing "can unzip known .gz file"
    (let [tmp-file (.toString ^File (fs/temp-file "unzipped-file"))]
      (unzip-file "dev-resources/fixtures/plain-text.txt.gz" tmp-file)
      (is (= (slurp tmp-file) (slurp "dev-resources/fixtures/plain-text.txt")))))
  (testing "creates missing directories"
    (let [tmp-file (str (.toString ^File (fs/temp-dir "test-directory")) "/foo/bar/baz/thing.txt")]
      (unzip-file "dev-resources/fixtures/plain-text.txt.gz" tmp-file)
      (is (= (slurp tmp-file) (slurp "dev-resources/fixtures/plain-text.txt")))))
  (testing "throws if file does not exist"
    (let [tmp-file (.toString ^File (fs/temp-file "not-unzipped-file"))]
      (is (thrown? FileNotFoundException (unzip-file "something" tmp-file)))))
  (testing "refuses to overwrite input file"
    (let [tmp-file (.toString ^File (fs/temp-file "copy-of-file"))]
      (io/copy (io/file "dev-resources/fixtures/plain-text.txt.gz") (io/file tmp-file))
      (unzip-file tmp-file tmp-file)
      (is (= (slurp "dev-resources/fixtures/plain-text.txt.gz") (slurp tmp-file))))))