(ns puppetlabs.kitchensink.time-test
  (:require [clojure.test :refer :all])
  (:require [puppetlabs.kitchensink.time :as time])
  (:import (java.time Duration)))

(deftest duration-str->seconds-test
  (testing "returns an integer when given an integer"
    (are [x] (= x (time/duration-str->seconds x))
            -1 0 1 2 3 4 5 6 7 8 9 10))
  (testing "returns expected values for simple input"
    (are [x y] (= x (time/duration-str->seconds y))
               94608000 "3y"
               259200 "3d"
               10800 "3h"
               180 "3m"
               3 "3s"))
  (testing "Throws exceptions on invalid input"
    (is (thrown? IllegalArgumentException (time/duration-str->seconds "6y3m3y")))
    (is (thrown? IllegalArgumentException (time/duration-str->seconds "3y6d7d8d3d")))
    (is (thrown? IllegalArgumentException (time/duration-str->seconds "3y3y6d7d3h")))
    (is (thrown? IllegalArgumentException (time/duration-str->seconds "3y6y5h6d3m")))
    (is (thrown? IllegalArgumentException (time/duration-str->seconds "3y6y5h6d3m3s")))
    (is (thrown? IllegalArgumentException (time/duration-str->seconds "not-a-duration")))))

(deftest duration-str->Duration-test
  (testing "returns a duration with the expected number of seconds when given an integer"
    (testing "returns an integer when given an integer"
      (are [x] (= (Duration/ofSeconds x) (time/duration-str->Duration x))
                 -1 0 1 2 3 4 5 6 7 8 9 10))
    (testing "returns expected values for simple input"
      (are [x y] (= (Duration/ofSeconds x) (time/duration-str->Duration y))
                 94608000 "3y"
                 259200 "3d"
                 10800 "3h"
                 180 "3m"
                 3 "3s"))
    (testing "Throws exceptions on invalid input"
      (is (thrown? IllegalArgumentException (time/duration-str->Duration "6y3m3y")))
      (is (thrown? IllegalArgumentException (time/duration-str->Duration "3y6d7d8d3d")))
      (is (thrown? IllegalArgumentException (time/duration-str->Duration "3y3y6d7d3h")))
      (is (thrown? IllegalArgumentException (time/duration-str->Duration "3y6y5h6d3m")))
      (is (thrown? IllegalArgumentException (time/duration-str->Duration "3y6y5h6d3m3s")))
      (is (thrown? IllegalArgumentException (time/duration-str->Duration "not-a-duration"))))))

(deftest days->hours-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/days->hours y))
               0 0
               -24 -1
               24 1
               48 2
               (* 24 1.1) 1.1)))

(deftest hours->days-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/hours->days y))
               0 0
               -1 -24
               1 24
               2 48
               1.1 (* 24 1.1))))

(deftest hours->min-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/hours->min y))
               0 0
               -60 -1
               60 1
               120 2
               (* 60 1.1) 1.1)))

(deftest min->hours-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/min->hours y))
               0 0
               -1 -60
               1 60
               2 120
               1.1 (* 60 1.1))))

(deftest min->sec-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/min->sec y))
               0 0
               -60 -1
               60 1
               120 2
               (* 60 1.1) 1.1)))

(deftest sec->min-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/sec->min y))
               0 0
               -1 -60
               1 60
               2 120
               1.1 (* 60 1.1))))

(deftest sec->ms-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/sec->ms y))
               0 0
               -1000 -1
               1000 1
               2000 2
               1100.0 1.1)))

(deftest ms->sec-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/ms->sec y))
               0 0
               -1 -1000
               1 1000
               2 2000
               11/10 1100)))

(deftest min->ms-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/min->ms y))
               0 0
               -60000 -1
               60000 1
               120000 2
               66000.0 1.1)))

(deftest ms->min-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/ms->min y))
               0 0
               -1 -60000
               1 60000
               2 120000
               11/10 66000)))

(deftest hours->ms-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/hours->ms y))
               0 0
               -3600000 -1
               3600000 1
               7200000 2
               3960000.0 1.1)))

(deftest ms->hours-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/ms->hours y))
               0 0
               -1 -3600000
               1 3600000
               2 7200000
               11/10 3960000)))

(deftest days->ms-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/days->ms y))
               0 0
               -86400000 -1
               86400000 1
               172800000 2
               (* 1.1 24 60 60 1000) 1.1)))

(deftest ms->days-test
  (testing "expected input provides expected output"
    (are [x y] (= x (time/ms->days y))
               0 0
               -1 -86400000
               1 86400000
               2 172800000
               1.1 (* 1.1 24 60 60 1000))))
