(ns puppetlabs.kitchensink.classpath-test
  (:require [clojure.test :refer :all]
            [puppetlabs.kitchensink.classpath :refer [with-additional-classpath-entries]])
  (:import (java.net URL)))

(deftest with-additional-classpath-entries-test
  (let [paths ["/foo" "/bar"]
        get-urls #(into #{}
                        (.getURLs (.getContextClassLoader (Thread/currentThread))))]
    (with-additional-classpath-entries
      paths
      (testing "classloader now includes the new paths"
        (let [urls (get-urls)]
          (is (contains? urls (URL. "file:/foo")))
          (is (contains? urls (URL. "file:/bar"))))))
    (testing "classloader has been restored to its previous state"
      (let [urls (get-urls)]
        (is (not (contains? urls (URL. "file:/foo"))))
        (is (not (contains? urls (URL. "file:/bar"))))))))
