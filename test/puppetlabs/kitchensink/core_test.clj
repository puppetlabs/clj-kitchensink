(ns puppetlabs.kitchensink.core-test
  (:require [clojure.test :refer :all]
            [puppetlabs.kitchensink.core :refer :all]
            [me.raynes.fs :as fs]
            [slingshot.slingshot :refer [try+]]
            [clojure.string :as string]
            [puppetlabs.kitchensink.testutils :as testutils]
            [clojure.zip :as zip])
  (:import (java.util ArrayList)))

(deftest array?-test
  (testing "array?"

    (testing "should work for nil input"
      (is (nil? (array? nil))))

    (testing "should detect primitive arrays"
      (doseq [f #{object-array boolean-array byte-array short-array char-array int-array long-array float-array double-array}]
        (is (true? (array? (f 1))))))

    (testing "should return nil for non-array objects"
      (doseq [x ['() [] {} "foo" 123 456.789 1/3]]
        (is (false? (array? x)))))))

(deftest boolean?-test
  (testing "should return true if true"
    (is (boolean? true)))
  (testing "should return true if false"
    (is (boolean? false)))
  (testing "should return false if string"
    (is (not (boolean? "test"))))
  (testing "should return false if nil"
    (is (not (boolean? nil)))))

(deftest regexp?-test
  (testing "should return true if pattern"
    (is (regexp? (re-pattern "test"))))
  (is (regexp? #"test"))
  (testing "should return false if string"
    (is (not (regexp? "test")))))

(deftest datetime?-test
  (testing "should return false for non-coercible types"
    (is (not (datetime? 2.0))))
  (testing "should return false for nil"
    (is (not (datetime? nil))))
  (testing "should return true for a valid string"
    (is (datetime? "2011-01-01T12:00:00-03:00")))
  (testing "should return false for an invalid string"
    (is (not (datetime? "foobar"))))
  (testing "should return true for a valid integer"
    (is (datetime? 20)))
  (testing "should return false for an invalid integer")
  (is (not (datetime? -9999999999999999999999999999999))))

(deftest zipper?-test
  (testing "should return true for zippers"
    (is (true? (zipper? (zip/vector-zip [:foo :bar])))))
  (testing "should return false for non-zippers"
    (is (false? (zipper? "hi")))
    (is (false? (zipper? 42)))
    (is (false? (zipper? :foo)))
    (is (false? (zipper? [:foo :bar])))
    (is (false? (zipper? {:foo :bar})))))

(deftest to-bool-test
  (testing "should return the same value when passed a Boolean"
    (is (true? (to-bool true)))
    (is (false? (to-bool false))))
  (testing "should return true or false when passed a string representation of same"
    (is (true? (to-bool "true")))
    (is (true? (to-bool "TRUE")))
    (is (true? (to-bool "tRuE")))
    (is (false? (to-bool "false")))
    (is (false? (to-bool "FALSE")))
    (is (false? (to-bool "fAlSe"))))
  (testing "should return false when passed nil"
    (is (false? (to-bool nil))))
  (testing "should throw an exception when passed a string other than true or false"
    (try+
      (to-bool "hi")
      (is (not true) "Expected exception to be thrown by to-bool when an invalid string is passed")
      (catch map? m
        (is (contains? m :kind))
        (is (= :puppetlabs.kitchensink.core/parse-error (:kind m)))
        (is (= :parse-error (without-ns (:kind m))))
        (is (contains? m :msg))
        (is (re-find #"Unable to parse 'hi' to a boolean" (:msg m)))))))

(deftest test-true-str?
  (are [t-or-f? str-val] (t-or-f? (true-str? str-val))

                         true? "true"
                         true? "TRUE"
                         true? "TrUe"

                         false? "false"
                         false? nil
                         false? "FALSE"))

(deftest to-sentence-test
  (are [coll string] (= string (to-sentence coll))
       [] ""
       ["foo"] "foo"
       ["foo" "bar"] "foo and bar"
       ["foo" "bar" "baz"] "foo, bar, and baz"
       ["foo" "bar" "baz" "qux"] "foo, bar, baz, and qux"))

(deftest mkdirs-test
  (testing "creates all specified directories that don't exist for File arg"
    (let [tmpdir (temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (mkdirs! (fs/file tmpdir "foo" "bar" "baz"))
      (is (fs/directory? (fs/file tmpdir "foo" "bar" "baz")))))
  (testing "creates all specified directories that don't exist for String arg"
    (let [tmpdir (temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (mkdirs! (.getPath (fs/file tmpdir "foo" "bar" "baz")))
      (is (fs/directory? (fs/file tmpdir "foo" "bar" "baz")))))
  (testing "throws exception if one of the elements of the path exists and is a file"
    (let [tmpdir (temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (fs/touch (fs/file tmpdir "foo" "bar"))
      (try+
        (mkdirs! (fs/file tmpdir "foo" "bar" "baz"))
        (is (not true) "Expected exception to be thrown by mkdirs! when one of the elements of the path already exists and is a file")
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/io-error (:kind m)))
          (is (= :io-error (without-ns (:kind m))))
          (is (contains? m :msg))
          (is (re-find #"foo/bar' is a file" (:msg m)))))))
  (testing "throws exception if the path exists and is a file"
    (let [tmpdir (temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (fs/touch (fs/file tmpdir "foo" "bar"))
      (try+
        (mkdirs! (fs/file tmpdir "foo" "bar"))
        (is (not true) (str "Expected exception to be thrown by mkdirs! when "
                            "the path already exists and is a file"))
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/io-error (:kind m)))
          (is (= :io-error (without-ns (:kind m))))
          (is (contains? m :msg))
          (is (re-find #"foo/bar' is a file" (:msg m)))))))
  (testing "Permission denied on some directory in the hierarchy"
    (let [tmpdir (temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (fs/chmod "-w" (fs/file tmpdir "foo"))
      (try+
        (mkdirs! (fs/file tmpdir "foo" "bar" "baz"))
        (is (not true) "Expected exception to be thrown by mkdirs! when a permissions error occurs")
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/io-error (:kind m)))
          (is (= :io-error (without-ns (:kind m))))
          (is (contains? m :msg))
          (is (re-find #"foo' is not writable" (:msg m))))))))

(deftest quotient-test
  (testing "quotient"

    (testing "should behave like '/' when divisor is non-zero"
      (is (= 22/7 (quotient 22 7))))

    (testing "should return default when divisor is zero"
      (is (= 0 (quotient 1 0)))
      (is (= 10 (quotient 1 0 10))))))

(deftest rand-weighted-selection-test
  (testing "rand-weighted-selection"

    (testing "should make selections within 1% of expected values when n=100k"
      (let [make-selection #(rand-weighted-selection
                              0.1 :foo
                              0.3 :bar
                              0.4 :baz
                              0.19 :quux
                              0.01 :waffle)
            n (int 1e5)
            freqs (frequencies (repeatedly n make-selection))
            expected-freqs {:foo 10000
                            :bar 30000
                            :baz 40000
                            :quux 19000
                            :waffle 1000}]
        (doseq [[value actual] freqs
                :let [expected (get expected-freqs value)]]
          (is (< (Math/abs (- expected actual)) (/ n 100))))))

    (testing "adjusts the weight of the last value when the weights sum to less than 1"
      (is (= :foo (rand-weighted-selection 0.0 :foo))))

    (testing "doesn't select values whose weights come after prior weights have sum to 1"
      (dotimes [_ 1e3]
        (is (not= :bar (rand-weighted-selection 1.0 :foo 10.0 :bar)))))

    (testing "throws an error when"

      (testing "a weight does not have a value"
        (is (thrown? AssertionError (rand-weighted-selection 0.0))))

      (testing "a weight is not numeric"
        (is (thrown? AssertionError (rand-weighted-selection :foo :bar)))))))

(deftest excludes?-test
  (testing "should return true if coll does not contain key"
    (is (excludes? {:foo 1} :bar)))
  (testing "should return false if coll does contain key"
    (is (not (excludes? {:foo 1} :foo)))))

(deftest contains-some-test
  (testing "should return nil if coll doesn't contain any of the keys"
    (is (= nil (contains-some {:foo 1} [:bar :baz :bam]))))
  (testing "should return the first key that coll does contain"
    (is (= :baz (contains-some {:foo 1 :baz 2 :bam 3} [:bar :baz :bam])))))

(deftest excludes-some-test
  (testing "should return nil if coll does `contain?` all of the keys"
    (is (= nil (excludes-some {:bar 1 :baz 2} [:bar :baz]))))
  (testing "should return the first key that coll does *not* `contain?`"
    (is (= :baz (excludes-some {:bar 1 :foo 2} [:foo :baz :bam])))))

(deftest mapvals-test
  (testing "should default to applying a function to all of the keys"
    (is (= {:a 2 :b 3} (mapvals inc {:a 1 :b 2}))))
  (testing "should support applying a function to a subset of the keys"
    (is (= {:a 2 :b 2} (mapvals inc [:a] {:a 1 :b 2}))))
  (testing "should support keywords as the function to apply to all of the keys"
    (is (= {:a 1 :b 2} (mapvals :foo {:a {:foo 1} :b {:foo 2}}))))
  (testing "should support keywords as the function to apply to a subset of the keys"
    (is (= {:a 1 :b {:foo 2}} (mapvals :foo [:a] {:a {:foo 1} :b {:foo 2}})))))

(deftest maptrans-test
  (testing "should fail if the keys-fns param isn't valid"
    (is (thrown? AssertionError (maptrans "blah" {:a 1 :b 1}))))
  (testing "should transform a map based on the given functions"
    (is (= {:a 3 :b 3 :c 3 :d 3}
          (maptrans {[:a :b] inc [:d] dec} {:a 2 :b 2 :c 3 :d 4}))))
  (testing "should accept keywords as functions in the keys-fns param"
    (is (= {:a 3 :b 3}
          (maptrans {[:a :b] :foo} {:a {:foo 3} :b {:foo 3}})))))

(deftest dissoc-if-nil-test
  (let [testmap {:a 1 :b nil}]
    (testing "should remove the key if the value is nil"
      (is (= (dissoc testmap :b) (dissoc-if-nil testmap :b))))
    (testing "should not remove the key if the value is not nil"
      (is (= testmap (dissoc-if-nil testmap :a))))))

(deftest dissoc-in-test
  (let [testmap {:a {:b 1 :c {:d 2}}}]
    (testing "should remove the key"
      (is (= {:a {:c {:d 2}}} (dissoc-in testmap [:a :b]))))
    (testing "should remove the empty map"
      (is (= {:a {:b 1}} (dissoc-in testmap [:a :c :d]))))))

(deftest walk-leaves-test
  (testing "should apply a function to all of the leaves"
    (is (= {:a 2 :b {:c 5}} (walk-leaves {:a 1 :b {:c 4}} inc)))))

(deftest merge-with-key-test
  (let [m1        {:a 1 :b 2}
        m2        {:a 3 :b 4}
        merge-fn  (fn [k v1 v2]
                    (if (= k :a)
                      (+ v1 v2)
                      v2))]
    (is (= {:a 4 :b 4} (merge-with-key merge-fn m1 m2)))))

(deftest deep-merge-test
  (testing "should deeply nest duplicate keys that both have map values"
    (let [testmap-1 {:foo {:bar :baz}, :pancake :flapjack}
          testmap-2 {:foo {:fuzz {:buzz :quux}}}]
      (is (= {:foo {:bar :baz, :fuzz {:buzz :quux}}, :pancake :flapjack}
             (deep-merge testmap-1 testmap-2)))))
  (testing "should combine duplicate keys' values that aren't all maps by
           calling the provided function"
    (let [testmap-1 {:foo {:bars 2}}
          testmap-2 {:foo {:bars 3, :bazzes 4}}]
      (is (= {:foo {:bars 5, :bazzes 4}} (deep-merge-with + testmap-1 testmap-2)))))
  (testing "deep-merge-with-keys should pass keys to specified fn"
    (let [m1        {:a {:b 1 :c 2}}
          m2        {:a {:b 3 :c 4}}
          merge-fn  (fn [ks v1 v2]
                       (if (= ks [:a :b])
                         (+ v1 v2)
                         v2))]
      (is (= {:a {:b 4 :c 4}} (deep-merge-with-keys merge-fn m1 m2))))))

(deftest filter-map-test
  (testing "should filter based on a given predicate"
    (let [test-map {:dog 5 :cat 4 :mouse 7 :cow 6}]
      (is (= (filter-map (fn [k v] (even? v)) test-map)
             {:cat 4, :cow 6}))
      (is (= (filter-map (fn [k v] (= 3 (count (name k)))) test-map)
             {:dog 5, :cat 4, :cow 6}))
      (is (= (filter-map (fn [k v] (and (= 3 (count (name k))) (> v 5))) test-map)
             {:cow 6}))
      (is (= (filter-map (fn [k v] true) test-map)
             test-map))
      (is (= (filter-map (fn [k v] false) test-map)
             {}))))
  (testing "should return empty map if given nil"
    (is (= {} (filter-map nil nil)))))

(deftest missing?-test
  (let [sample {:a "asdf" :b "asdf" :c "asdf"}]
    (testing "should return true for single key items if they don't exist in the coll"
      (is (true? (missing? sample :n))))
    (testing "should return false for single key items if they exist in the coll"
      (is (false? (missing? sample :c))))
    (testing "should return true for multiple key items if they all don't exist in the coll"
      (is (true? (missing? sample :n :f :g :z :h))))
    (testing "should return false for multiple key items if one item exists in the coll"
      (is (false? (missing? sample :n :b :f)))
      (is (false? (missing? sample :a :h :f))))
    (testing "should return false for multiple key items if all items exist in the coll"
      (is (false? (missing? sample :a :b :c))))))

(deftest order-by-test
  (let [test-data [{:id 1
                    :k1 "ALPHA"
                    :k2 "BETA"
                    :k3 "GAMMA"}
                   {:id 2
                    :k1 "ALPHA"
                    :k2 "BETA"
                    :k3 "EPSILON"}
                   {:id 3
                    :k1 "ALPHA"
                    :k2 "CHARLIE"
                    :k3 "DELTA"}
                   {:id 4
                    :k1 "ALPHA"
                    :k2 "CHARLIE"
                    :k3 "FOXTROT"}
                   {:id 5
                    :k1 "alpha"
                    :k2 "beta"
                    :k3 "alpha"}]]
    (testing "single field, ascending order-by"
      (is (= [3 2 4 1 5] (map :id (order-by [[:k3 :ascending]] test-data)))))
    (testing "single field, descending order-by"
      (is (= [5 1 4 2 3] (map :id (order-by [[:k3 :descending]] test-data)))))
    (testing "single function, descending order-by"
      (is (= [1 4 2 3 5] (map :id (order-by [[#(string/lower-case (:k3 %)) :descending]] test-data)))))
    (testing "multiple order-bys"
      (is (= [5 3 4 2 1] (map :id
                           (order-by
                             [[#(string/upper-case (:k1 %)) :ascending]
                              [:k2 :descending]
                              [:k3 :ascending]]
                             test-data)))))))

(deftest sort-nested-maps-test
  (testing "with nested structure"
    (let [input  {:b "asdf"
                  :a {:z "asdf"
                      :k [:z {:z 26 :a 1} :c]
                      :a {:m 12 :a 1}
                      :b "asdf"}}
          output (sort-nested-maps input)]
      (testing "after sorting, maps should still match"
        (is (= input output)))
      (testing "all maps levels of output should be sorted"
        (is (sorted? output))
        (is (sorted? (:a output)))
        (is (sorted? (get (vec (get-in output [:a :k])) 1)))
        (is (sorted? (get-in output [:a :a]))))))
  (testing "with a string"
    (let [input "string here"
          output (sort-nested-maps input)]
      (testing "should match"
        (is (= input output)))))
  (testing "with a list"
    (let [input '(:a :b :c)
          output (sort-nested-maps input)]
      (testing "should still match"
        (is (= input output))))))

(deftest without-ns-test
  (testing "removes namespace from a namespaced keyword"
    (is (= :foo (without-ns :foo/foo)))
    (is (= :foo (without-ns ::foo))))
  (testing "doesn't alter non-namespaced keyword"
    (let [kw :foo]
      (is (= kw (without-ns kw))))))

(deftest string-hashing
  (testing "Computing a SHA-1 for a UTF-8 string"
    (testing "should fail if not passed a string"
      (is (thrown? AssertionError (utf8-string->sha1 1234))))

    (testing "should produce a stable hash"
      (is (= (utf8-string->sha1 "foobar")
            (utf8-string->sha1 "foobar"))))

    (testing "should produce the correct hash"
      (is (= "8843d7f92416211de9ebb963ff4ce28125932878"
            (utf8-string->sha1 "foobar"))))))

(deftest temp-file-name-test
  (testing "The file should not exist."
    (is (not (fs/exists? (temp-file-name "foo")))))
  (testing "It should be possible to create a file at the given path."
    (is (fs/create (temp-file-name "foo")))))

(deftest temp-file-test
  (testing "should create a temp file when not given a prefix"
    (let [f (temp-file)]
      (is (fs/file? f))))
  (testing "should create a temp file when given a prefix and suffix"
    (let [f (temp-file "foo" ".bar")]
      (is (fs/file? f))
      (is (.startsWith (.getName f) "foo"))
      (is (.endsWith (.getName f) ".bar"))))
  (testing "should create a temp dir when not given a prefix"
    (let [d (temp-dir)]
      (is (fs/directory? d))))
  (testing "should create a temp dir when given a prefix and suffix"
    (let [d (temp-dir "foo" ".bar")]
      (is (fs/directory? d))
      (is (.startsWith (.getName d) "foo"))
      (is (.endsWith (.getName d) ".bar")))))


(deftest ini-parsing
  (testing "Parsing ini files"
    (testing "should work for a single file"
      (let [tf (temp-file)]
        (spit tf "[foo]\nbar=baz")

        (testing "when specified as a file object"
          (is (= (inis-to-map tf)
                {:foo {:bar "baz"}})))

        (testing "when specified as a string"
          (is (= (inis-to-map (absolute-path tf))
                {:foo {:bar "baz"}})))))

    (testing "should work for a directory"
      (let [td (temp-dir)]
        (testing "when no matching files exist"
          (is (= (inis-to-map td) {})))

        (let [tf (fs/file td "a-test.ini")]
          (spit tf "[foo]\nbar=baz"))

        (testing "when only a single matching file exists"
          (is (= (inis-to-map td)
                {:foo {:bar "baz"}})))

        (let [tf (fs/file td "b-test.ini")]
          ;; Now add a second file
          (spit tf "[bar]\nbar=baz"))

        (testing "when multiple matching files exist"
          (is (= (inis-to-map td)
                {:foo {:bar "baz"}
                 :bar {:bar "baz"}})))))))

(deftest cli-parsing
  (testing "Should throw an error if a required option is missing"
    (let [got-expected-error (atom false)]
      (try+
        (cli! [] [["-r" "--required" "A required field"]] [:required])
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/cli-error (:kind m)))
          (is (= :cli-error (without-ns (:kind m))))
          (is (contains? m :msg))
          (reset! got-expected-error true)))
      (is (true? @got-expected-error))))

  (testing "Should throw a help message if --help is provided"
    (let [got-expected-help (atom false)]
      (try+
        (cli! ["--help"] [] [])
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/cli-help (:kind m)))
          (is (= :cli-help (without-ns (:kind m))))
          (is (contains? m :msg))
          (reset! got-expected-help true)))
      (is (true? @got-expected-help))))

  (testing "Should return options map, remaining args, and summary after parsing CLI args"
    (let [[cli-data remaining-args summary] (cli! ["-a" "1234 Sunny ave." "--greeting" "Hey, what's up?" "--toggle" "extra-arg"]
                                               [["-g" "--greeting GREETING" "A string to greet somebody"]
                                                ["-a" "--address ADDRESS" "Somebody's address"]
                                                ["-t" "--toggle" "A flag/boolean option"]] [])]
      (is (map? cli-data))
      (is (= "1234 Sunny ave." (cli-data :address)))
      (is (= "Hey, what's up?" (cli-data :greeting)))
      (is (= true (cli-data :toggle)))
      (is (vector? remaining-args))
      (is (= ["extra-arg"] remaining-args))
      (is (= (str "  -g, --greeting GREETING  A string to greet somebody\n"
                  "  -a, --address ADDRESS    Somebody's address\n"
                  "  -t, --toggle             A flag/boolean option\n"
                  "  -h, --help               Show help")
             summary))))

  (testing "Errors reported by tools.cli should be thrown out of cli! as slingshot exceptions"
    (let [got-expected-exception (atom false)]
      (try+
        (let [specs [["-f" "--foo FOO" "Something that is foo"]]
              args  ["--bar"]]
          (cli! args specs))
        (catch map? m
          (is (= :puppetlabs.kitchensink.core/cli-error (:kind m)))
          (is (contains? m :msg))
          (is (re-find
                #"Unknown option.*--bar"
                (m :msg)))
          (reset! got-expected-exception true)))
      (is (true? @got-expected-exception)))))

(deftest cert-utils
  (testing "extracting cn from a dn"
    (is (thrown? AssertionError (cn-for-dn 123))
      "should throw error when arg is a number")
    (is (thrown? AssertionError (cn-for-dn nil))
      "should throw error when arg is nil")

    (is (= (cn-for-dn "") nil)
      "should return nil when passed an empty string")
    (is (= (cn-for-dn "MEH=bar") nil)
      "should return nil when no CN is present")
    (is (= (cn-for-dn "cn=foo.bar.com") nil)
      "should return nil when CN present but lower case")
    (is (= (cn-for-dn "cN=foo.bar.com") nil)
      "should return nil when CN present but with mixed case")

    (is (= (cn-for-dn "CN=foo.bar.com") "foo.bar.com")
      "should work when only CN is present")
    (is (= (cn-for-dn "CN=foo.bar.com,OU=something") "foo.bar.com")
      "should work when more than just the CN is present")
    (is (= (cn-for-dn "CN=foo.bar.com,OU=something") "foo.bar.com")
      "should work when more than just the CN is present")
    (is (= (cn-for-dn "OU=something,CN=foo.bar.com") "foo.bar.com")
      "should work when more than just the CN is present and CN is last")
    (is (= (cn-for-dn "OU=something,CN=foo.bar.com,D=foobar") "foo.bar.com")
      "should work when more than just the CN is present and CN is in the middle")
    (is (= (cn-for-dn "CN=foo.bar.com,CN=goo.bar.com,OU=something") "goo.bar.com")
      "should use the most specific CN if multiple CN's are present")))

(deftest cert-whitelist-auth
  (testing "cert whitelist authorizer"
    (testing "should fail when whitelist is not given"
      (is (thrown? AssertionError (cn-whitelist->authorizer nil))))

    (testing "should fail when whitelist is given, but not readable"
      (is (thrown? java.io.FileNotFoundException
            (cn-whitelist->authorizer "/this/does/not/exist"))))

    (testing "when whitelist is present"
      (let [whitelist (temp-file)]
        (spit whitelist "foo\nbar\n")

        (let [authorized? (cn-whitelist->authorizer whitelist)]
          (testing "should allow plain-text, HTTP requests"
            (is (authorized? {:scheme :http :ssl-client-cn "foobar"})))

          (testing "should fail HTTPS requests without a client cert"
            (is (not (authorized? {:scheme :https}))))

          (testing "should reject certs that don't appear in the whitelist"
            (is (not (authorized? {:scheme :https :ssl-client-cn "goo"}))))

          (testing "should accept certs that appear in the whitelist"
            (is (authorized? {:scheme :https :ssl-client-cn "foo"}))))))))

(deftest memoization
  (testing "with an illegal bound"
    (is (thrown? AssertionError (bounded-memoize identity -1)))
    (is (thrown? AssertionError (bounded-memoize identity 0)))
    (is (thrown? AssertionError (bounded-memoize identity 1.5)))
    (is (thrown? AssertionError (bounded-memoize identity "five"))))

  (testing "with a legal bound"
    (let [f (testutils/call-counter)
          memoized (bounded-memoize f 2)]
      (testing "should only call the function once per argument"
        (is (= (testutils/times-called f) 0))

        (memoized 0)
        (is (= (testutils/times-called f) 1))

        (memoized 0)
        (is (= (testutils/times-called f) 1))

        (memoized 1)
        (is (= (testutils/times-called f) 2)))

      ;; We call it here for a hit, which we expect not to clear the cache,
      ;; then call it again to verify the cache wasn't cleared and therefore f
      ;; wasn't called
      (testing "should not clear the cache at max size on a hit"
        (memoized 1)
        (is (= (testutils/times-called f) 2))

        (memoized 1)
        (is (= (testutils/times-called f) 2)))

      ;; Now call it with a new argument to clear the cache, then with an old
      ;; one to show f is called
      (testing "should clear the cache at max size on a miss"
        (memoized 2)
        (is (= (testutils/times-called f) 3))

        (memoized 3)
        (is (= (testutils/times-called f) 4))))))

(deftest uuid-handling
  (testing "a generated uuid is a valid uuid"
    (is (uuid? (uuid))))
  (testing "a phrase is not a uuid"
    (is (not (uuid? "Hello World")))))

(deftest jvm-versions
  (testing "comparing same versions should return 0"
    (is (= 0 (compare-jvm-versions "1.7.0_3" "1.7.0_3"))))

  (testing "comparing same versions should return 0, even with trailing fields"
    (is (= 0 (compare-jvm-versions "1.7.0_3" "1.7.0_3-beta3"))))

  (testing "should detect older versions"
    (is (neg? (compare-jvm-versions "1.7.0_0" "1.7.0_3")))
    (is (neg? (compare-jvm-versions "1.7.0_0" "1.7.0_3-beta3")))
    (is (neg? (compare-jvm-versions "1.7.0_2" "1.7.0_03")))
    (is (neg? (compare-jvm-versions "1.6.0_3" "1.7.0_3")))
    (is (neg? (compare-jvm-versions "1.6.0_2" "1.7.0_3")))
    (is (neg? (compare-jvm-versions "0.6.0_2" "1.7.0_3"))))

  (testing "should detect newer versions"
    (is (pos? (compare-jvm-versions "1.7.0_13" "1.7.0_3")))
    (is (pos? (compare-jvm-versions "1.8.0_3" "1.7.0_3")))
    (is (pos? (compare-jvm-versions "1.8.0_3" "1.7.0_3-beta3")))
    (is (pos? (compare-jvm-versions "2.7.0_3" "1.7.0_3")))
    (is (pos? (compare-jvm-versions "1.7.0_10" "1.7.0_3")))))

(deftest some-pred->>-macro
  (testing "should thread all the way through if the pred never matches"
    (is (= 10
          (some-pred->> nil? 1
            (* 2)
            (+ 9)
            (dec)))))
  (testing "should break and return the value if the pred matches"
    (is (= {:a 1}
          (some-pred->> map? 5
            (/ 5)
            (assoc {} :a)
            (keys))))))

(deftest while-let-macro
  (let [counter (atom 0)
        list (ArrayList.)]
    (dotimes [_ 5] (.add list "foo"))
    (let [iter (.iterator list)]
      (while-let [item (and (.hasNext iter)
                            (.next iter))]
        (swap! counter inc)))
    (is (= 5 @counter))))

(deftest test-spit-ini
  (let [tf (temp-file)]
    (spit tf "[foo]\nbar=baz\n[bar]\nfoo=baz")
    (let [ini-map (ini-to-map tf)]
      (is (= ini-map
            {:foo {:bar "baz"}
             :bar {:foo "baz"}}))
      (testing "changing existing keys"
        (let [result-file (temp-file)]
          (spit-ini result-file (-> ini-map
                                  (assoc-in [:foo :bar] "baz changed")
                                  (assoc-in [:bar :foo] "baz also changed")))
          (is (= {:foo {:bar "baz changed"}
                  :bar {:foo "baz also changed"}}
                (ini-to-map result-file)))))
      (testing "adding a new section to an existing ini"
        (let [result-file (temp-file)]
          (spit-ini result-file (assoc-in ini-map [:baz :foo] "bar"))
          (is (= {:foo {:bar "baz"}
                  :bar {:foo "baz"}
                  :baz {:foo "bar"}}
                (ini-to-map result-file))))))))

(deftest duplicate-ini-entries
  (testing "duplicate settings"
    (let [tempfile (temp-file)]
      (spit tempfile "[foo]\nbar=baz\nbar=bizzle\n")
      (is (thrown-with-msg?
            IllegalArgumentException
            #"Duplicate configuration entry: \[:foo :bar\]"
            (ini-to-map tempfile))))

    (let [tempdir   (temp-dir)
          tempfile1 (fs/file tempdir "initest1.ini")
          tempfile2 (fs/file tempdir "initest2.ini")]
      (spit tempfile1 "[foo]\nsetting1=hi\nbar=baz\n")
      (spit tempfile2 "[foo]\nsetting2=hi\nbar=bizzle\n")
      (is (thrown-with-msg?
            IllegalArgumentException
            #"Duplicate configuration entry: \[:foo :bar\]"
            (inis-to-map tempdir)))))

  (testing "duplicate sections but no duplicate settings"
    (let [tempdir   (temp-dir)
          tempfile1 (fs/file tempdir "initest1.ini")
          tempfile2 (fs/file tempdir "initest.ini")]
      (spit tempfile1 "[foo]\nsetting1=hi\nbar=baz\n")
      (spit tempfile2 "[foo]\nsetting2=hi\nbunk=bizzle\n")
      (is (= {:foo {:setting1 "hi"
                    :bar      "baz"
                    :setting2 "hi"
                    :bunk     "bizzle"}}
             (inis-to-map tempdir))))))

(deftest timeout-test
  (let [wait-return (fn [time val] (Thread/sleep time) val)]
    (testing "with-timeout"
      (testing "does nothing if the body returns within the limit"
        (is (= true
               (with-timeout 1 false
                 (wait-return 500 true)))))
      (testing "returns the default value if the body times out"
        (is (= false
               (with-timeout 1 false
                 (wait-return 1005 true))))))))

(deftest open-port-num-test
  (let [port-in-use (open-port-num)]
    (with-open [s (java.net.ServerSocket. port-in-use)]
      (let [open-ports (set (take 60000 (repeatedly open-port-num)))]
        (is (every? pos? open-ports))
        (is (not (contains? open-ports port-in-use)))))))

(deftest assoc-if-new-test
  (testing "assoc-if-new assocs appropriately"
    (is (= {:a "foo"}
           (assoc-if-new {:a "foo"} :a "bar")))
    (is (= {:a "bar" :b "foo"}
           (assoc-if-new {:b "foo"}  :a "bar")))
    (is (= {:a "foo" :b "bar"}
           (assoc-if-new {} :a "foo" :b "bar")))
    (is (= {:a "foo" :b nil}
           (assoc-if-new {:b nil} :a "foo" :b "bar")))
    (is (= {:a "foo" :b "baz"}
           (assoc-if-new {:b "baz"} :a "foo" :b "bar")))))
