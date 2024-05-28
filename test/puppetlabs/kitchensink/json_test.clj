(ns puppetlabs.kitchensink.json-test
  (:require
    [clj-time.core :as clj-time]
    [puppetlabs.kitchensink.core :refer [temp-file]]
    [puppetlabs.kitchensink.json :as ks-json])
  (:use [clojure.test])
  (:import
    (com.fasterxml.jackson.core JsonGenerator)
    (java.io StringReader StringWriter)
    (java.time Instant LocalDate LocalDateTime)))

(defn add-common-encoders-fixture
  [f]
  (ks-json/add-common-json-encoders!*)
  (f))

(use-fixtures :once add-common-encoders-fixture)

(deftest test-with-custom-datetime-encoder
  (testing "should allow use of custom encoder"
    (is (= (ks-json/with-datetime-encoder (fn [_dt gn8r] (.writeString ^JsonGenerator gn8r "Beer-o-clock"))
             (ks-json/generate-string (clj-time/date-time 1989 11 17 5 6 24 654)))
           "\"Beer-o-clock\""))))

(deftest test-generate-string
  (testing "should generate a json string"
    (is (= (ks-json/generate-string {:a 1 :b 2})
           "{\"a\":1,\"b\":2}")))
  (testing "should generate a json string that has a Joda DataTime object in it and not explode"
    (is (= (ks-json/generate-string {:a 1 :b (clj-time/date-time 1986 10 14 4 3 27 456)})
           "{\"a\":1,\"b\":\"1986-10-14T04:03:27.456Z\"}")))
  (testing "should generate a json string that has a java.time.Instant in it and not explode"
    (is (= (ks-json/generate-string {:a 1 :b (Instant/parse "1994-09-19T21:00:30Z")})
           "{\"a\":1,\"b\":\"1994-09-19T21:00:30Z\"}")))
  (testing "should generate a json string that has a java.time.LocalDate in it and not explode"
    (is (= (ks-json/generate-string {:a 1 :b (LocalDate/parse "1953-05-29")})
           "{\"a\":1,\"b\":\"1953-05-29\"}")))
  (testing "should generate a json string that has a java.time.Instant in it and not explode"
    (is (= (ks-json/generate-string {:a 1 :b (LocalDateTime/parse "1954-07-31T21:00:30")})
           "{\"a\":1,\"b\":\"1954-07-31T21:00:30\"}"))))

(deftest test-generate-pretty-string
  (testing "should generate a json string"
    (is (= (ks-json/generate-pretty-string {:a 1 :b 2})
           "{\n  \"a\" : 1,\n  \"b\" : 2\n}")))
  (testing "should generate a json string that has a Joda DataTime object in it and not explode"
    (is (= (ks-json/generate-pretty-string {:a 1 :b (clj-time/date-time 1986 10 14 4 3 27 456)})
           "{\n  \"a\" : 1,\n  \"b\" : \"1986-10-14T04:03:27.456Z\"\n}")))
  (testing "should generate a json string that has a java.time.Instant in it and not explode"
    (is (= (ks-json/generate-pretty-string {:a 1 :b (Instant/parse "1994-09-19T21:00:30Z")})
           "{\n  \"a\" : 1,\n  \"b\" : \"1994-09-19T21:00:30Z\"\n}")))
  (testing "should generate a json string that has a java.time.LocalDate in it and not explode"
    (is (= (ks-json/generate-pretty-string {:a 1 :b (LocalDate/parse "1953-05-29")})
           "{\n  \"a\" : 1,\n  \"b\" : \"1953-05-29\"\n}")))
  (testing "should generate a json string that has a java.time.Instant in it and not explode"
    (is (= (ks-json/generate-pretty-string {:a 1 :b (LocalDateTime/parse "1954-07-31T21:00:30")})
           "{\n  \"a\" : 1,\n  \"b\" : \"1954-07-31T21:00:30\"\n}"))))

(deftest test-generate-stream
  (testing "should generate a json string from a stream"
    (let [sw (StringWriter.)]
      (ks-json/generate-stream {:a 1 :b 2} sw)
      (is (= (.toString sw)
             "{\"a\":1,\"b\":2}")))))

(deftest test-generate-pretty-stream
  (testing "should generate a pretty printed json string from a stream"
    (let [sw (StringWriter.)]
      (ks-json/generate-pretty-stream {:a 1 :b 2} sw)
      (is (= (.toString sw)
             "{\n  \"a\" : 1,\n  \"b\" : 2\n}")))))

(deftest test-parse-string
  (testing "should return a map from parsing a json string"
    (is (= (ks-json/parse-string "{\"a\":1,\"b\":2}")
           {"a" 1 "b" 2}))))

(deftest test-parse-stream
  (testing "should return  map from parsing a json stream"
    (is (= (ks-json/parse-stream (StringReader. "{\"a\":1,\"b\":2}"))
           {"a" 1 "b" 2}))))

(deftest test-spit-json
  (let [json-out (temp-file "spit-json")]
    (testing "json output with keywords"
      (ks-json/spit-json json-out {:a 1 :b 2})
      (is (= "{\n  \"a\" : 1,\n  \"b\" : 2\n}"
             (slurp json-out))))
    (testing "json output with strings"
      (ks-json/spit-json json-out {"a" 1 "b" 2})
      (is (= "{\n  \"a\" : 1,\n  \"b\" : 2\n}"
             (slurp json-out))))))
