(ns puppetlabs.kitchensink.time-test
  (:require [clojure.test :refer :all])
  (:require [puppetlabs.kitchensink.time :refer [duration-str->seconds duration-str->Duration]])
  (:import (java.time Duration)))

(deftest duration-str->seconds-test
  (testing "returns an integer when given an integer"
    (are [x] (= x (duration-str->seconds x))
            -1 0 1 2 3 4 5 6 7 8 9 10))
  (testing "returns expected values for simple input"
    (are [x y] (= x (duration-str->seconds y))
               94608000 "3y"
               259200 "3d"
               10800 "3h"
               180 "3m"
               3 "3s"))
  (testing "Throws exceptions on invalid input"
    (is (thrown? IllegalArgumentException (duration-str->seconds "6y3m3y")))
    (is (thrown? IllegalArgumentException (duration-str->seconds "3y6d7d8d3d")))
    (is (thrown? IllegalArgumentException (duration-str->seconds "3y3y6d7d3h")))
    (is (thrown? IllegalArgumentException (duration-str->seconds "3y6y5h6d3m")))
    (is (thrown? IllegalArgumentException (duration-str->seconds "3y6y5h6d3m3s")))
    (is (thrown? IllegalArgumentException (duration-str->seconds "not-a-duration")))))

(deftest duration-str->Duration-test
  (testing "returns a duration with the expected number of seconds when given an integer"
    (testing "returns an integer when given an integer"
      (are [x] (= (Duration/ofSeconds x) (duration-str->Duration x))
                 -1 0 1 2 3 4 5 6 7 8 9 10))
    (testing "returns expected values for simple input"
      (are [x y] (= (Duration/ofSeconds x) (duration-str->Duration y))
                 94608000 "3y"
                 259200 "3d"
                 10800 "3h"
                 180 "3m"
                 3 "3s"))
    (testing "Throws exceptions on invalid input"
      (is (thrown? IllegalArgumentException (duration-str->Duration "6y3m3y")))
      (is (thrown? IllegalArgumentException (duration-str->Duration "3y6d7d8d3d")))
      (is (thrown? IllegalArgumentException (duration-str->Duration "3y3y6d7d3h")))
      (is (thrown? IllegalArgumentException (duration-str->Duration "3y6y5h6d3m")))
      (is (thrown? IllegalArgumentException (duration-str->Duration "3y6y5h6d3m3s")))
      (is (thrown? IllegalArgumentException (duration-str->Duration "not-a-duration"))))))
