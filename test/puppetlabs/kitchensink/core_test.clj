(ns puppetlabs.kitchensink.core-test
  (:require [clj-time.core :as t]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.zip :as zip]
            [me.raynes.fs :as fs]
            [puppetlabs.kitchensink.core :as core]
            [puppetlabs.kitchensink.testutils :as testutils]
            [slingshot.slingshot :refer [try+]])
  (:import (java.io ByteArrayInputStream File)
           (java.time Month ZonedDateTime)
           (java.time.format DateTimeParseException)
           (java.util ArrayList)))

(deftest array?-test
  (testing "array?"

    (testing "should work for nil input"
      (is (nil? (core/array? nil))))

    (testing "should detect primitive arrays"
      (doseq [f #{object-array boolean-array byte-array short-array char-array int-array long-array float-array double-array}]
        (is (true? (core/array? (f 1))))))

    (testing "should return nil for non-array objects"
      (doseq [x ['() [] {} "foo" 123 456.789 1/3]]
        (is (false? (core/array? x)))))))

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
    (is (core/regexp? (re-pattern "test"))))
  (is (core/regexp? #"test"))
  (testing "should return false if string"
    (is (not (core/regexp? "test")))))

(deftest datetime?-test
  (testing "should return false for non-coercible types"
    (is (not (core/datetime? 2.0))))
  (testing "should return false for nil"
    (is (not (core/datetime? nil))))
  (testing "should return true for a valid string"
    (is (core/datetime? "2011-01-01T12:00:00-03:00")))
  (testing "should return false for an invalid string"
    (is (not (core/datetime? "foobar"))))
  (testing "should return true for a valid integer"
    (is (core/datetime? 20)))
  (testing "should return false for an invalid integer")
  (is (not (core/datetime? -9999999999999999999999999999999))))

(deftest zipper?-test
  (testing "should return true for zippers"
    (is (true? (core/zipper? (zip/vector-zip [:foo :bar])))))
  (testing "should return false for non-zippers"
    (is (false? (core/zipper? "hi")))
    (is (false? (core/zipper? 42)))
    (is (false? (core/zipper? :foo)))
    (is (false? (core/zipper? [:foo :bar])))
    (is (false? (core/zipper? {:foo :bar})))))

(deftest to-bool-test
  (testing "should return the same value when passed a Boolean"
    (is (true? (core/to-bool true)))
    (is (false? (core/to-bool false))))
  (testing "should return true or false when passed a string representation of same"
    (is (true? (core/to-bool "true")))
    (is (true? (core/to-bool "TRUE")))
    (is (true? (core/to-bool "tRuE")))
    (is (false? (core/to-bool "false")))
    (is (false? (core/to-bool "FALSE")))
    (is (false? (core/to-bool "fAlSe"))))
  (testing "should return false when passed nil"
    (is (false? (core/to-bool nil))))
  (testing "should throw an exception when passed a string other than true or false"
    (try+
      (core/to-bool "hi")
      (is (not true) "Expected exception to be thrown by core/to-bool when an invalid string is passed")
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (catch map? m
        (is (contains? m :kind))
        (is (= :puppetlabs.kitchensink.core/parse-error (:kind m)))
        (is (= :parse-error (core/without-ns (:kind m))))
        (is (contains? m :msg))
        (is (re-find #"Unable to parse 'hi' to a boolean" (:msg m)))))))

(deftest true-str?-test
  (are [t-or-f? str-val] (t-or-f? (core/true-str? str-val))

                         true? "true"
                         true? "TRUE"
                         true? "TrUe"

                         false? "false"
                         false? nil
                         false? "FALSE"))

(deftest to-sentence-test
  (are [coll string] (= string (core/to-sentence coll))
       [] ""
       ["foo"] "foo"
       ["foo" "bar"] "foo and bar"
       ["foo" "bar" "baz"] "foo, bar, and baz"
       ["foo" "bar" "baz" "qux"] "foo, bar, baz, and qux"))

(deftest key->str-test
  (testing "returns strings unmodified"
    (is (= "foo" (core/key->str "foo")))
    (is (= ":foo" (core/key->str ":foo")))
    (is (= "foo/bar" (core/key->str "foo/bar"))))

  (testing "returns the entire stringified keyword"
    (is (= "foo" (core/key->str :foo)))
    (is (= ":foo" (core/key->str (keyword ":foo"))))
    (is (= "foo/bar" (core/key->str :foo/bar)))))

(deftest mkdirs-test
  (testing "creates all specified directories that don't exist for File arg"
    (let [tmpdir (core/temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (core/mkdirs! (fs/file tmpdir "foo" "bar" "baz"))
      (is (fs/directory? (fs/file tmpdir "foo" "bar" "baz")))))
  (testing "creates all specified directories that don't exist for String arg"
    (let [tmpdir (core/temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (core/mkdirs! (.getPath (fs/file tmpdir "foo" "bar" "baz")))
      (is (fs/directory? (fs/file tmpdir "foo" "bar" "baz")))))
  (testing "throws exception if one of the elements of the path exists and is a file"
    (let [tmpdir (core/temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (fs/touch (fs/file tmpdir "foo" "bar"))
      (try+
        (core/mkdirs! (fs/file tmpdir "foo" "bar" "baz"))
        (is (not true) "Expected exception to be thrown by core/mkdirs! when one of the elements of the path already exists and is a file")
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/io-error (:kind m)))
          (is (= :io-error (core/without-ns (:kind m))))
          (is (contains? m :msg))
          (is (re-find #"foo/bar' is a file" (:msg m)))))))
  (testing "throws exception if the path exists and is a file"
    (let [tmpdir (core/temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (fs/touch (fs/file tmpdir "foo" "bar"))
      (try+
        (core/mkdirs! (fs/file tmpdir "foo" "bar"))
        (is (not true) (str "Expected exception to be thrown by core/mkdirs! when "
                            "the path already exists and is a file"))
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/io-error (:kind m)))
          (is (= :io-error (core/without-ns (:kind m))))
          (is (contains? m :msg))
          (is (re-find #"foo/bar' is a file" (:msg m)))))))
  (testing "Permission denied on some directory in the hierarchy"
    (let [tmpdir (core/temp-dir)]
      (fs/mkdirs (fs/file tmpdir "foo"))
      (fs/chmod "-w" (fs/file tmpdir "foo"))
      (try+
        (core/mkdirs! (fs/file tmpdir "foo" "bar" "baz"))
        (is (not true) "Expected exception to be thrown by core/mkdirs! when a permissions error occurs")
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/io-error (:kind m)))
          (is (= :io-error (core/without-ns (:kind m))))
          (is (contains? m :msg))
          (is (re-find #"foo' is not writable" (:msg m))))))))

(deftest quotient-test
  (testing "core/quotient"

    (testing "should behave like '/' when divisor is non-zero"
      (is (= 22/7 (core/quotient 22 7))))

    (testing "should return default when divisor is zero"
      (is (= 0 (core/quotient 1 0)))
      (is (= 10 (core/quotient 1 0 10))))))

(deftest rand-weighted-selection-test
  (testing "core/rand-weighted-selection"

    (testing "should make selections within 1% of expected values when n=100k"
      (let [make-selection #(core/rand-weighted-selection
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
          (is (< (Math/abs ^int (- expected actual)) (/ n 100))))))

    (testing "adjusts the weight of the last value when the weights sum to less than 1"
      (is (= :foo (core/rand-weighted-selection 0.0 :foo))))

    (testing "doesn't select values whose weights come after prior weights have sum to 1"
      (dotimes [_ 1e3]
        (is (not= :bar (core/rand-weighted-selection 1.0 :foo 10.0 :bar)))))

    (testing "throws an error when"

      (testing "a weight does not have a value"
        (is (thrown? AssertionError (core/rand-weighted-selection 0.0))))

      (testing "a weight is not numeric"
        (is (thrown? AssertionError (core/rand-weighted-selection :foo :bar)))))))

(deftest rand-str-test
  (testing "core/rand-str"
    (testing "throws an IllegalArgumentException when given an unknown characters keyword"
      (is (thrown-with-msg? IllegalArgumentException #":CJK" (core/rand-str :CJK 42))))

    (doseq [[kw cs] core/ascii-character-sets
            :let [cs (set cs)]]
      (testing (str "recognizes the " kw " character set keyword")
        (dotimes [_ 10]
          (is (every? cs (core/rand-str kw 1000))))))

    (testing "uses collections of strings & characters as character sets"
      (let [as ["a" \a]]
        (dotimes [_ 100]
          (is (every? #(= % \a) (core/rand-str as 100))))))))

(deftest excludes?-test
  (testing "should return true if coll does not contain key"
    (is (core/excludes? {:foo 1} :bar)))
  (testing "should return false if coll does contain key"
    (is (not (core/excludes? {:foo 1} :foo)))))

(deftest contains-some-test
  (testing "should return nil if coll doesn't contain any of the keys"
    (is (= nil (core/contains-some {:foo 1} [:bar :baz :bam]))))
  (testing "should return the first key that coll does contain"
    (is (= :baz (core/contains-some {:foo 1 :baz 2 :bam 3} [:bar :baz :bam])))))

(deftest excludes-some-test
  (testing "should return nil if coll does `contain?` all of the keys"
    (is (= nil (core/excludes-some {:bar 1 :baz 2} [:bar :baz]))))
  (testing "should return the first key that coll does *not* `contain?`"
    (is (= :baz (core/excludes-some {:bar 1 :foo 2} [:foo :baz :bam])))))

(deftest mapvals-test
  (testing "should default to applying a function to all of the keys"
    (is (= {:a 2 :b 3} (core/mapvals inc {:a 1 :b 2}))))
  (testing "should support applying a function to a subset of the keys"
    (is (= {:a 2 :b 2} (core/mapvals inc [:a] {:a 1 :b 2}))))
  (testing "should support keywords as the function to apply to all of the keys"
    (is (= {:a 1 :b 2} (core/mapvals :foo {:a {:foo 1} :b {:foo 2}}))))
  (testing "should support keywords as the function to apply to a subset of the keys"
    (is (= {:a 1 :b {:foo 2}} (core/mapvals :foo [:a] {:a {:foo 1} :b {:foo 2}})))))

(deftest maptrans-test
  (testing "should fail if the keys-fns param isn't valid"
    (is (thrown? AssertionError (core/maptrans "blah" {:a 1 :b 1}))))
  (testing "should transform a map based on the given functions"
    (is (= {:a 3 :b 3 :c 3 :d 3}
          (core/maptrans {[:a :b] inc [:d] dec} {:a 2 :b 2 :c 3 :d 4}))))
  (testing "should accept keywords as functions in the keys-fns param"
    (is (= {:a 3 :b 3}
          (core/maptrans {[:a :b] :foo} {:a {:foo 3} :b {:foo 3}})))))

(deftest dissoc-if-nil-test
  (let [testmap {:a 1 :b nil}]
    (testing "should remove the key if the value is nil"
      (is (= (dissoc testmap :b) (core/dissoc-if-nil testmap :b))))
    (testing "should not remove the key if the value is not nil"
      (is (= testmap (core/dissoc-if-nil testmap :a))))))

(deftest dissoc-in-test
  (let [testmap {:a {:b 1 :c {:d 2}}}]
    (testing "should remove the key"
      (is (= {:a {:c {:d 2}}} (core/dissoc-in testmap [:a :b]))))
    (testing "should remove the empty map"
      (is (= {:a {:b 1}} (core/dissoc-in testmap [:a :c :d]))))))

(deftest walk-leaves-test
  (testing "should apply a function to all of the leaves"
    (is (= {:a 2 :b {:c 5}} (core/walk-leaves {:a 1 :b {:c 4}} inc)))))

(deftest merge-with-key-test
  (let [m1        {:a 1 :b 2}
        m2        {:a 3 :b 4}
        merge-fn  (fn [k v1 v2]
                    (if (= k :a)
                      (+ v1 v2)
                      v2))]
    (is (= {:a 4 :b 4} (core/merge-with-key merge-fn m1 m2)))))

(deftest deep-merge-test
  (testing "should deeply nest duplicate keys that both have map values"
    (let [testmap-1 {:foo {:bar :baz}, :pancake :flapjack}
          testmap-2 {:foo {:fuzz {:buzz :quux}}}]
      (is (= {:foo {:bar :baz, :fuzz {:buzz :quux}}, :pancake :flapjack}
             (core/deep-merge testmap-1 testmap-2)))))
  (testing "should combine duplicate keys' values that aren't all maps by
           calling the provided function"
    (let [testmap-1 {:foo {:bars 2}}
          testmap-2 {:foo {:bars 3, :bazzes 4}}]
      (is (= {:foo {:bars 5, :bazzes 4}} (core/deep-merge-with + testmap-1 testmap-2)))))
  (testing "core/deep-merge-with-keys should pass keys to specified fn"
    (let [m1        {:a {:b 1 :c 2}}
          m2        {:a {:b 3 :c 4}}
          merge-fn  (fn [ks v1 v2]
                       (if (= ks [:a :b])
                         (+ v1 v2)
                         v2))]
      (is (= {:a {:b 4 :c 4}} (core/deep-merge-with-keys merge-fn m1 m2))))))

(deftest filter-map-test
  (testing "should filter based on a given predicate"
    (let [test-map {:dog 5 :cat 4 :mouse 7 :cow 6}]
      (is (= (core/filter-map (fn [_k v] (even? v)) test-map)
             {:cat 4, :cow 6}))
      (is (= (core/filter-map (fn [k _v] (= 3 (count (name k)))) test-map)
             {:dog 5, :cat 4, :cow 6}))
      (is (= (core/filter-map (fn [k v] (and (= 3 (count (name k))) (> v 5))) test-map)
             {:cow 6}))
      (is (= (core/filter-map (fn [_k _v] true) test-map)
             test-map))
      (is (= (core/filter-map (fn [_k _v] false) test-map)
             {}))))
  (testing "should return empty map if given nil"
    (is (= {} (core/filter-map nil nil)))))

(deftest missing?-test
  (let [sample {:a "asdf" :b "asdf" :c "asdf"}]
    (testing "should return true for single key items if they don't exist in the coll"
      (is (true? (core/missing? sample :n))))
    (testing "should return false for single key items if they exist in the coll"
      (is (false? (core/missing? sample :c))))
    (testing "should return true for multiple key items if they all don't exist in the coll"
      (is (true? (core/missing? sample :n :f :g :z :h))))
    (testing "should return false for multiple key items if one item exists in the coll"
      (is (false? (core/missing? sample :n :b :f)))
      (is (false? (core/missing? sample :a :h :f))))
    (testing "should return false for multiple key items if all items exist in the coll"
      (is (false? (core/missing? sample :a :b :c))))))

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
    (testing "single field, ascending core/order-by"
      (is (= [3 2 4 1 5] (map :id (core/order-by [[:k3 :ascending]] test-data)))))
    (testing "single field, descending core/order-by"
      (is (= [5 1 4 2 3] (map :id (core/order-by [[:k3 :descending]] test-data)))))
    (testing "single function, descending core/order-by"
      (is (= [1 4 2 3 5] (map :id (core/order-by [[#(string/lower-case (:k3 %)) :descending]] test-data)))))
    (testing "multiple core/order-bys"
      (is (= [5 3 4 2 1] (map :id
                           (core/order-by
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
          output (core/sort-nested-maps input)]
      (testing "after sorting, maps should still match"
        (is (= input output)))
      (testing "all maps levels of output should be sorted"
        (is (sorted? output))
        (is (sorted? (:a output)))
        (is (sorted? (get (vec (get-in output [:a :k])) 1)))
        (is (sorted? (get-in output [:a :a]))))))
  (testing "with a string"
    (let [input "string here"
          output (core/sort-nested-maps input)]
      (testing "should match"
        (is (= input output)))))
  (testing "with a list"
    (let [input '(:a :b :c)
          output (core/sort-nested-maps input)]
      (testing "should still match"
        (is (= input output))))))

(deftest without-ns-test
  (testing "removes namespace from a namespaced keyword"
    (is (= :foo (core/without-ns :foo/foo)))
    (is (= :foo (core/without-ns ::foo))))
  (testing "doesn't alter non-namespaced keyword"
    (let [kw :foo]
      (is (= kw (core/without-ns kw))))))

(deftest string-hashing-test
  (testing "Computing a SHA-1 for a UTF-8 string"
    (testing "should fail if not passed a string"
      #_{:clj-kondo/ignore [:type-mismatch]}
      (is (thrown? AssertionError (core/utf8-string->sha1 1234))))

    (testing "should produce a stable hash"
      (is (= (core/utf8-string->sha1 "foobar")
            (core/utf8-string->sha1 "foobar"))))

    (testing "should produce the correct hash"
      (is (= "8843d7f92416211de9ebb963ff4ce28125932878"
             (core/utf8-string->sha1 "foobar")))))

  (testing "Computing a SHA-256 for a UTF-8 string"
    (testing "should fail if not passed a string"
      #_{:clj-kondo/ignore [:type-mismatch]}
      (is (thrown? AssertionError (core/utf8-string->sha256 1234))))

    (testing "should produce a stable hash"
      (is (= (core/utf8-string->sha256 "foobar")
             (core/utf8-string->sha256 "foobar"))))

    (testing "should produce the correct hash"
      (is (= "c3ab8ff13720e8ad9047dd39466b3c8974e592c2fa383d4a3960714caef0c4f2"
             (core/utf8-string->sha256 "foobar"))))))

(deftest stream-hashing
  (testing "Computing a SHA-256 hash for an input stream"
    (testing "should fail if not passed an input stream"
      #_ {:clj-kondo/ignore [:type-mismatch]}
      (is (thrown? AssertionError (core/stream->sha256 "what"))))

    (let [stream-fn #(ByteArrayInputStream. (.getBytes "foobar" "UTF-8"))]
      (testing "should produce a stable hash"
        (is (= (core/stream->sha256 (stream-fn))
               (core/stream->sha256 (stream-fn)))))

      (testing "should produce the correct hash"
        (is (= "c3ab8ff13720e8ad9047dd39466b3c8974e592c2fa383d4a3960714caef0c4f2"
               (core/stream->sha256 (stream-fn))))))))

(deftest file-hashing
  (testing "Computing a SHA-256 hash for a file"
    (testing "should fail if not passed a file"
      (is (thrown? AssertionError (core/file->sha256 "what"))))

    (let [f (core/temp-file "sha256" ".txt")]
      (spit f "foobar")

      (testing "should produce a stable hash"
        (is (= (core/file->sha256 f)
               (core/file->sha256 f))))

      (testing "should produce the correct hash"
        (is (= "c3ab8ff13720e8ad9047dd39466b3c8974e592c2fa383d4a3960714caef0c4f2"
               (core/file->sha256 f)))))))

(deftest temp-file-name-test
  (testing "The file should not exist."
    (is (not (fs/exists? (core/temp-file-name "foo")))))
  (testing "It should be possible to create a file at the given path."
    (is (fs/create (core/temp-file-name "foo")))))

(deftest temp-file-test
  (testing "should create a temp file when not given a prefix"
    (let [f (core/temp-file)]
      (is (fs/file? f))))
  (testing "should create a temp file when given a prefix and suffix"
    (let [^File f (core/temp-file "foo" ".bar")]
      (is (fs/file? f))
      (is (.startsWith (.getName f) "foo"))
      (is (.endsWith (.getName f) ".bar"))))
  (testing "should create a temp dir when not given a prefix"
    (let [d (core/temp-dir)]
      (is (fs/directory? d))))
  (testing "should create a temp dir when given a prefix and suffix"
    (let [^File d (core/temp-dir "foo" ".bar")]
      (is (fs/directory? d))
      (is (.startsWith (.getName d) "foo"))
      (is (.endsWith (.getName d) ".bar")))))


(deftest ini-parsing
  (testing "Parsing ini files"
    (testing "should work for a single file"
      (let [tf (core/temp-file)]
        (spit tf "[foo]\nbar=baz")

        (testing "when specified as a file object"
          (is (= (core/inis-to-map tf)
                {:foo {:bar "baz"}})))

        (testing "when specified as a string"
          (is (= (core/inis-to-map (core/absolute-path tf))
                {:foo {:bar "baz"}})))))

    (testing "should work for a directory"
      (let [td (core/temp-dir)]
        (testing "when no matching files exist"
          (is (= (core/inis-to-map td) {})))

        (let [tf (fs/file td "a-test.ini")]
          (spit tf "[foo]\nbar=baz"))

        (testing "when only a single matching file exists"
          (is (= (core/inis-to-map td)
                {:foo {:bar "baz"}})))

        (let [tf (fs/file td "b-test.ini")]
          ;; Now add a second file
          (spit tf "[bar]\nbar=baz"))

        (testing "when multiple matching files exist"
          (is (= (core/inis-to-map td)
                {:foo {:bar "baz"}
                 :bar {:bar "baz"}})))))))

(deftest cli-parsing
  (testing "Should throw an error if a required option is missing"
    (let [got-expected-error (atom false)]
      (try+
        (core/cli! [] [["-r" "--required" "A required field"]] [:required])
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/cli-error (:kind m)))
          (is (= :cli-error (core/without-ns (:kind m))))
          (is (contains? m :msg))
          (reset! got-expected-error true)))
      (is (true? @got-expected-error))))

  (testing "Should throw a help message if --help is provided"
    (let [got-expected-help (atom false)]
      (try+
        (core/cli! ["--help"] [] [])
        (catch map? m
          (is (contains? m :kind))
          (is (= :puppetlabs.kitchensink.core/cli-help (:kind m)))
          (is (= :cli-help (core/without-ns (:kind m))))
          (is (contains? m :msg))
          (reset! got-expected-help true)))
      (is (true? @got-expected-help))))

  (testing "Should return options map, remaining args, and summary after parsing CLI args"
    (let [[cli-data remaining-args summary] (core/cli! ["-a" "1234 Sunny ave." "--greeting" "Hey, what's up?" "--toggle" "extra-arg"]
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
          (core/cli! args specs))
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
    #_{:clj-kondo/ignore [:type-mismatch]}
    (is (thrown? AssertionError (core/cn-for-dn 123))
      "should throw error when arg is a number")
    (is (thrown? AssertionError (core/cn-for-dn nil))
      "should throw error when arg is nil")

    (is (= (core/cn-for-dn "") nil)
      "should return nil when passed an empty string")
    (is (= (core/cn-for-dn "MEH=bar") nil)
      "should return nil when no CN is present")
    (is (= (core/cn-for-dn "cn=foo.bar.com") nil)
      "should return nil when CN present but lower case")
    (is (= (core/cn-for-dn "cN=foo.bar.com") nil)
      "should return nil when CN present but with mixed case")

    (is (= (core/cn-for-dn "CN=foo.bar.com") "foo.bar.com")
      "should work when only CN is present")
    (is (= (core/cn-for-dn "CN=foo.bar.com,OU=something") "foo.bar.com")
      "should work when more than just the CN is present")
    (is (= (core/cn-for-dn "CN=foo.bar.com,OU=something") "foo.bar.com")
      "should work when more than just the CN is present")
    (is (= (core/cn-for-dn "OU=something,CN=foo.bar.com") "foo.bar.com")
      "should work when more than just the CN is present and CN is last")
    (is (= (core/cn-for-dn "OU=something,CN=foo.bar.com,D=foobar") "foo.bar.com")
      "should work when more than just the CN is present and CN is in the middle")
    (is (= (core/cn-for-dn "CN=foo.bar.com,CN=goo.bar.com,OU=something") "goo.bar.com")
      "should use the most specific CN if multiple CN's are present")))

(deftest cert-whitelist-auth
  (testing "cert whitelist authorizer"
    (testing "should fail when whitelist is not given"
      (is (thrown? AssertionError (core/cn-whitelist->authorizer nil))))

    (testing "should fail when whitelist is given, but not readable"
      (is (thrown? java.io.FileNotFoundException
            (core/cn-whitelist->authorizer "/this/does/not/exist"))))

    (testing "when whitelist is present"
      (let [whitelist (core/temp-file)]
        (spit whitelist "foo\nbar\n")

        (let [authorized? (core/cn-whitelist->authorizer whitelist)]
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
    (is (thrown? AssertionError (core/bounded-memoize identity -1)))
    (is (thrown? AssertionError (core/bounded-memoize identity 0)))
    (is (thrown? AssertionError (core/bounded-memoize identity 1.5)))
    (is (thrown? AssertionError (core/bounded-memoize identity "five"))))

  (testing "with a legal bound"
    (let [f (testutils/call-counter)
          memoized (core/bounded-memoize f 2)]
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
  (testing "a generated core/uuid is a valid core/uuid"
    (is (core/uuid? (core/uuid))))
  (testing "a phrase is not a core/uuid"
    (is (not (core/uuid? "Hello World")))))

(deftest jvm-versions
  (testing "comparing same versions should return 0"
    (is (= 0 (core/compare-jvm-versions "1.7.0_3" "1.7.0_3"))))

  (testing "comparing same versions should return 0, even with trailing fields"
    (is (= 0 (core/compare-jvm-versions "1.7.0_3" "1.7.0_3-beta3"))))

  (testing "should detect older versions"
    (is (neg? (core/compare-jvm-versions "1.7.0_0" "1.7.0_3")))
    (is (neg? (core/compare-jvm-versions "1.7.0_0" "1.7.0_3-beta3")))
    (is (neg? (core/compare-jvm-versions "1.7.0_2" "1.7.0_03")))
    (is (neg? (core/compare-jvm-versions "1.6.0_3" "1.7.0_3")))
    (is (neg? (core/compare-jvm-versions "1.6.0_2" "1.7.0_3")))
    (is (neg? (core/compare-jvm-versions "0.6.0_2" "1.7.0_3"))))

  (testing "should detect newer versions"
    (is (pos? (core/compare-jvm-versions "1.7.0_13" "1.7.0_3")))
    (is (pos? (core/compare-jvm-versions "1.8.0_3" "1.7.0_3")))
    (is (pos? (core/compare-jvm-versions "1.8.0_3" "1.7.0_3-beta3")))
    (is (pos? (core/compare-jvm-versions "2.7.0_3" "1.7.0_3")))
    (is (pos? (core/compare-jvm-versions "1.7.0_10" "1.7.0_3")))))

(deftest some-pred->>-macro
  (testing "should thread all the way through if the pred never matches"
    (is (= 10
          (core/some-pred->> nil? 1
            (* 2)
            (+ 9)
            #_ {:clj-kondo/ignore [:invalid-arity]}
            (dec)))))
  (testing "should break and return the value if the pred matches"
    (is (= {:a 1}
          (core/some-pred->> map? 5
            (/ 5)
            #_ {:clj-kondo/ignore [:invalid-arity]}
            (assoc {} :a)
            #_ {:clj-kondo/ignore [:invalid-arity]}
            (keys))))))

(deftest while-let-macro
  (let [counter (atom 0)
        list (ArrayList.)]
    (dotimes [_ 5] (.add list "foo"))
    (let [iter (.iterator list)]
      #_ {:clj-kondo/ignore [:unresolved-symbol]}
      (core/while-let [item (and (.hasNext iter)
                                 (.next iter))]
        (swap! counter inc)))
    (is (= 5 @counter))))

(deftest spit-ini-test
  (let [tf (core/temp-file)]
    (spit tf "[foo]\nbar=baz\n[bar]\nfoo=baz")
    (let [ini-map (core/ini-to-map tf)]
      (is (= ini-map
            {:foo {:bar "baz"}
             :bar {:foo "baz"}}))
      (testing "changing existing keys"
        (let [result-file (core/temp-file)]
          (core/spit-ini result-file (-> ini-map
                                       (assoc-in [:foo :bar] "baz changed")
                                       (assoc-in [:bar :foo] "baz also changed")))
          (is (= {:foo {:bar "baz changed"}
                  :bar {:foo "baz also changed"}}
                (core/ini-to-map result-file)))))
      (testing "adding a new section to an existing ini"
        (let [result-file (core/temp-file)]
          (core/spit-ini result-file (assoc-in ini-map [:baz :foo] "bar"))
          (is (= {:foo {:bar "baz"}
                  :bar {:foo "baz"}
                  :baz {:foo "bar"}}
                (core/ini-to-map result-file))))))))

(deftest duplicate-ini-entries
  (testing "duplicate settings"
    (let [tempfile (core/temp-file)]
      (spit tempfile "[foo]\nbar=baz\nbar=bizzle\n")
      (is (thrown-with-msg?
            IllegalArgumentException
            #"Duplicate configuration entry: \[:foo :bar\]"
            (core/ini-to-map tempfile))))

    (let [tempdir   (core/temp-dir)
          tempfile1 (fs/file tempdir "initest1.ini")
          tempfile2 (fs/file tempdir "initest2.ini")]
      (spit tempfile1 "[foo]\nsetting1=hi\nbar=baz\n")
      (spit tempfile2 "[foo]\nsetting2=hi\nbar=bizzle\n")
      (is (thrown-with-msg?
            IllegalArgumentException
            #"Duplicate configuration entry: \[:foo :bar\]"
            (core/inis-to-map tempdir)))))

  (testing "duplicate sections but no duplicate settings"
    (let [tempdir   (core/temp-dir)
          tempfile1 (fs/file tempdir "initest1.ini")
          tempfile2 (fs/file tempdir "initest.ini")]
      (spit tempfile1 "[foo]\nsetting1=hi\nbar=baz\n")
      (spit tempfile2 "[foo]\nsetting2=hi\nbunk=bizzle\n")
      (is (= {:foo {:setting1 "hi"
                    :bar      "baz"
                    :setting2 "hi"
                    :bunk     "bizzle"}}
             (core/inis-to-map tempdir))))))

(deftest timeout-test
  (let [wait-return (fn [time val] (Thread/sleep time) val)]
    (testing "core/with-timeout"
      (testing "does nothing if the body returns within the limit"
        (is (= true
               (core/with-timeout 1 false
                 (wait-return 500 true)))))
      (testing "returns the default value if the body times out"
        (is (= false
               (core/with-timeout 1 false
                 (wait-return 1005 true))))))))

(deftest ^:slow open-port-num-test
  (let [port-in-use (core/open-port-num)]
    (with-open [_s (java.net.ServerSocket. port-in-use)]
      (let [open-ports (set (take 60000 (repeatedly core/open-port-num)))]
        (is (every? pos? open-ports))
        (is (not (contains? open-ports port-in-use)))))))

(deftest assoc-if-new-test
  (testing "assoc-if-new assocs appropriately"
    (is (= {:a "foo"}
           (core/assoc-if-new {:a "foo"} :a "bar")))
    (is (= {:a "bar" :b "foo"}
           (core/assoc-if-new {:b "foo"}  :a "bar")))
    (is (= {:a "foo" :b "bar"}
           (core/assoc-if-new {} :a "foo" :b "bar")))
    (is (= {:a "foo" :b nil}
           (core/assoc-if-new {:b nil} :a "foo" :b "bar")))
    (is (= {:a "foo" :b "baz"}
           (core/assoc-if-new {:b "baz"} :a "foo" :b "bar")))))

(deftest deref-swap-test
  (testing "deref-swap behaves as advertised"
    (let [a (atom 10)
          b (core/deref-swap! a inc)]
      (is (= 11 @a))
      (is (= 10 b)))))

(deftest parse-interval-test
  (are [x y] (= x (core/parse-interval y))
    (t/seconds 11) "11s"
    (t/minutes 12) "12m"
    (t/hours 13) "13h"
    (t/days 14) "14d"
    (t/years 15) "15y"
    (t/seconds 0) "0"
    (t/seconds 10) "10"
    nil "15a"
    nil "h"
    nil "12hhh"
    nil "12H"
    nil "1,300y"
    nil ""
    nil nil))

(deftest base-type-test
  (is (= "application/json" (core/base-type "application/json")))
  (is (= "application/json" (core/base-type "application/json;charset=UTF-8")))
  (is (= "application/json" (core/base-type "application/json ; charset=UTF-8")))

  (is (= "foo/bar" (core/base-type "foo/bar;someparam=baz")))

  (is (nil? (core/base-type "application/json:charset=UTF-8")))
  (is (nil? (core/base-type "appl=ication/json ; charset=UTF-8"))))

(deftest now->timestamp-string-test
  (let [result (core/now->timestamp-string)]
    (is (re-matcher #"202\d-\d\d-\d\dT\d\d:\d\d:\d\d\.\d" result))))

(deftest timestamp-string->ZonedDateTime-test
  (testing "throws exceptions for invalid timestamps"
    (is (thrown? DateTimeParseException (core/timestamp-string->ZonedDateTime "not a timestamp"))))
  (testing "correctly converts timestamps to local date times"
    (let [result (core/timestamp-string->ZonedDateTime "2023-06-14T17:27:51.732102Z")]
      (is (instance? ZonedDateTime result))
      (is (= 2023 (.getYear result)))
      (is (= Month/JUNE (.getMonth result)))
      (is (= 14 (.getDayOfMonth result)))
      (is (= 17 (.getHour result)))
      (is (= 27 (.getMinute result)))
      (is (= 51 (.getSecond result)))
      (is (= "Z" (.toString (.getZone result))))))
  (testing "respects time zone"
    (let [result (core/timestamp-string->ZonedDateTime "2023-06-14T17:27:51.732102+01:00")]
      (is (instance? ZonedDateTime result))
      (is (= 2023 (.getYear result)))
      (is (= Month/JUNE (.getMonth result)))
      (is (= 14 (.getDayOfMonth result)))
      (is (= 17 (.getHour result)))
      (is (= 27 (.getMinute result)))
      (is (= 51 (.getSecond result)))
      (is (= "+01:00" (.toString (.getZone result)))))))


(deftest ZonedDateTime->utc-ZonedDateTime-test
  (testing "does not impact timezone in utc"
    (let [result (core/ZonedDateTime->utc-ZonedDateTime (core/timestamp-string->ZonedDateTime "2023-06-14T17:27:51.732102Z"))]
      (is (instance? ZonedDateTime result))
      (is (= 2023 (.getYear result)))
      (is (= Month/JUNE (.getMonth result)))
      (is (= 14 (.getDayOfMonth result)))
      (is (= 17 (.getHour result)))
      (is (= 27 (.getMinute result)))
      (is (= 51 (.getSecond result)))
      (is (= "UTC" (.toString (.getZone result))))))
  (testing "converts non UTC to UtC"
    (let [result (core/ZonedDateTime->utc-ZonedDateTime (core/timestamp-string->ZonedDateTime "2023-06-14T17:27:51.732102+01:00"))]
      (is (instance? ZonedDateTime result))
      (is (= 2023 (.getYear result)))
      (is (= Month/JUNE (.getMonth result)))
      (is (= 14 (.getDayOfMonth result)))
      (is (= 16 (.getHour result)))
      (is (= 27 (.getMinute result)))
      (is (= 51 (.getSecond result)))
      (is (= "UTC" (.toString (.getZone result)))))))

(deftest is-only-number?-test
  (testing "returns false for things that aren't just numbers"
    (is (false? (core/is-only-number?  "one")))
    (is (false? (core/is-only-number?  "1one")))
    (is (false? (core/is-only-number?  "one1")))
    (is (false? (core/is-only-number?  "1 1")))
    (is (false? (core/is-only-number?  "-1")))
    (is (false? (core/is-only-number?  "1.1"))))
  (testing "returns true for things that are just numbers"
    (is (true? (core/is-only-number?  "1")))
    (is (true? (core/is-only-number?  "100000")))
    (is (true? (core/is-only-number?  "0")))))


(deftest starts-with-zero?-test
  (testing "returns false for strings that don't start with zero"
    (is (false? (core/starts-with-zero? "one")))
    (is (false? (core/starts-with-zero? "1one")))
    (is (false? (core/starts-with-zero? "1one0")))
    (is (false? (core/starts-with-zero? "1 0"))))
  (testing "returns true for strings that start with zero"
    (is (true? (core/starts-with-zero? "0one")))
    (is (true? (core/starts-with-zero? "01one")))
    (is (true? (core/starts-with-zero? "01one0")))
    (is (true? (core/starts-with-zero? "0 1 0")))))

(deftest compare-versions-test
  (testing "expected input produces expected output"
    (is (zero? (core/compare-versions "1.2" "1.2")))
    (is (neg? (core/compare-versions "1.2" "1.3")))
    (is (pos? (core/compare-versions "1.3" "1.2")))
    (is (zero? (core/compare-versions "0002" "0002")))
    (is (neg? (core/compare-versions "0002" "1")))
    (is (neg? (core/compare-versions "0002" "1.06")))
    (is (neg? (core/compare-versions "0002" "1.1-3")))
    (is (neg? (core/compare-versions "0002" "1.1-6")))
    (is (neg? (core/compare-versions "0002" "1.1.a")))
    (is (pos? (core/compare-versions "1" "0002")))
    (is (pos? (core/compare-versions "1.06" "0002")))
    (is (pos? (core/compare-versions "1.1-3" "0002")))
    (is (pos? (core/compare-versions "1.1-6" "0002")))
    (is (pos? (core/compare-versions "1.1.a" "0002")))
    (is (zero? (core/compare-versions "1" "1")))
    (is (neg? (core/compare-versions "1" "1.06")))
    (is (neg? (core/compare-versions "1" "1.1-3")))
    (is (neg? (core/compare-versions "1" "1.1-6")))
    (is (neg? (core/compare-versions "1" "1.1.a")))
    (is (neg? (core/compare-versions "1" "2")))
    (is (neg? (core/compare-versions "1" "2.0")))
    (is (pos? (core/compare-versions "1.06" "1")))
    (is (pos? (core/compare-versions "1.1-3" "1")))
    (is (pos? (core/compare-versions "1.1-6" "1")))
    (is (pos? (core/compare-versions "1.1.a" "1")))
    (is (pos? (core/compare-versions "2" "1")))
    (is (pos? (core/compare-versions "2.0" "1")))
    (is (zero? (core/compare-versions "1.1-3" "1.1-3")))
    (is (neg? (core/compare-versions "1.1-3" "1.1-6")))
    (is (neg? (core/compare-versions "1.1-3" "1.1.a")))
    (is (neg? (core/compare-versions "1.1-3" "2")))
    (is (neg? (core/compare-versions "1.1-3" "2.0")))
    (is (pos? (core/compare-versions "1.1-6" "1.1-3")))
    (is (pos? (core/compare-versions "1.1.a" "1.1-3")))
    (is (pos? (core/compare-versions "2" "1.1-3")))
    (is (pos? (core/compare-versions "2.0" "1.1-3")))
    (is (zero? (core/compare-versions "1.1.6" "1.1.6")))
    (is (neg? (core/compare-versions "1.1.6" "1.1.a")))
    (is (neg? (core/compare-versions "1.1.6" "2")))
    (is (neg? (core/compare-versions "1.1.6" "2.0")))
    (is (pos? (core/compare-versions "1.1.a" "1.1.6")))
    (is (pos? (core/compare-versions "2" "1.1.6")))
    (is (pos? (core/compare-versions "2.0" "1.1.6")))
    (is (zero? (core/compare-versions "1.1.a" "1.1.a")))
    (is (neg? (core/compare-versions "1.1.a" "2")))
    (is (neg? (core/compare-versions "1.1.a" "2.0")))
    (is (pos? (core/compare-versions "2" "1.1.a")))
    (is (zero? (core/compare-versions "2" "2")))
    (is (zero? (core/compare-versions "2.0" "2.0")))
    ;; this might not be expected, but it is the behavior.
    (is (pos? (core/compare-versions "2.0" "2")))
    (is (pos? (core/compare-versions "2.0" "1.1.a")))
    (is (neg? (core/compare-versions "2.4" "2.4b")))
    (is (pos? (core/compare-versions "2.4b" "2.4a")))))

(deftest get-lein-project-version-test
  (testing "unknown project returns nil"
    (is (nil? (core/get-lein-project-version "unknown"))))
  (testing "known project returns something"
    (is (string? (core/get-lein-project-version "kitchensink")))
    (is (not (string/blank? (core/get-lein-project-version "kitchensink"))))))
