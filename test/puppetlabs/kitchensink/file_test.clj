(ns puppetlabs.kitchensink.file-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [puppetlabs.kitchensink.file :refer :all]))

(deftest atomic-write-test
  (testing "when the file doesn't exist"
    (let [tmp-file (.toString (fs/temp-file "atomic-writes-test"))
          content "Something said, not good"]
      (fs/delete tmp-file)
      (atomic-write tmp-file #(.write % content))

      (testing "it writes the data"
        (is (= content (slurp tmp-file))))

      (testing "it applies a default mode of 0640"
        (is (= "rw-r-----" (get-perms tmp-file))))

      (testing "it applies the specified mode"
        (fs/delete tmp-file)
        (atomic-write tmp-file #(.write % content) "rwxrwxrwx")
        (is (= "rwxrwxrwx" (get-perms tmp-file))))

      (testing "it can create a read-only file"
        (fs/delete tmp-file)
        (atomic-write tmp-file #(.write % content) "r--------")
        (is (= "r--------" (get-perms tmp-file))))

      (fs/delete tmp-file)))

  (testing "when overwriting"
    (let [tmp-file (.toString (fs/temp-file "atomic-writes-test"))
          content "Something said, not good"]
      (set-perms tmp-file "rwxr-x--x")

      (testing "it preserves existing permissions"
        (atomic-write tmp-file #(.write % content))
        (is (= "rwxr-x--x" (get-perms tmp-file))))

      (testing "it overwrites existing permissions if specified"
        (atomic-write tmp-file #(.write % content) "rw-rw----")
        (is (= "rw-rw----" (get-perms tmp-file))))

      (testing "it updates a read-only file"
        (set-perms tmp-file "r--------")
        (atomic-write tmp-file #(.write % content))
        (is (= content (slurp tmp-file))))

      (fs/delete tmp-file)))

  (testing "the supplied function is allowed to close the writer"
    (let [tmp-file (.toString (fs/temp-file "atomic-writes-test"))
          write-fn (fn [writer]
                     (.write writer "They called me slow")
                     (.close writer))]

      (atomic-write tmp-file write-fn)

      (fs/delete tmp-file))))
