(ns puppetlabs.kitchensink.classpath-test
  (:require [clojure.test :refer :all]
            [dynapath.dynamic-classpath :refer [classpath-urls]]
            [puppetlabs.kitchensink.classpath :refer [add-classpath
                                                      with-additional-classpath-entries]])
  (:import (java.net URL))
  (:refer-clojure :exclude (add-classpath)))

(deftest with-additional-classpath-entries-test
  (let [paths ["/foo" "/bar"]
        get-urls #(into #{}
                        (classpath-urls (.getContextClassLoader (Thread/currentThread))))]
    ;; Called for side effect of ensuring there's a modifiable classloader
    ;; without needing to make that function public
    (add-classpath "file:/nil")

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
