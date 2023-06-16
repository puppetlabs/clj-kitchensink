(ns puppetlabs.kitchensink.classpath-test
  (:require [clojure.test :refer :all]
            [puppetlabs.kitchensink.classpath :refer [with-additional-classpath-entries]]))

(deftest with-additional-classpath-entries-test
  (let [paths ["classpath-test"]
        get-resource #(-> (Thread/currentThread)
                          .getContextClassLoader
                          (.getResource "does-not-exist-anywhere-else"))]
    (with-additional-classpath-entries
      paths
      (testing "classloader now includes the new path"
        (is (get-resource))))

    (testing "classloader no longer includes the new path"
      (is (not (get-resource))))))
