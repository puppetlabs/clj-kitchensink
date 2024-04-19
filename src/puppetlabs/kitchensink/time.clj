(ns puppetlabs.kitchensink.time
  (:require [puppetlabs.kitchensink.core :as core])
  (:import (java.time Duration)))

(def matching-pattern #"^(\d+)(y|d|h|m|s)?$")
(def unit-map {"y" (* 365 24 60 60),                        ; 365 days isn't technically a year, but is sufficient for most purposes
               "d" (* 24 60 60),
               "h" (* 60 60),
               "m" 60,
               "s" 1})

(defn duration-str->seconds
  "Given a puppet duration string, see https://github.com/puppetlabs/puppet/blob/fb44fd90c64ee11f0a29fb8924adbab5b0695555/lib/puppet/settings/duration_setting.rb
  for a reference implementation, return the duration in seconds"
  [duration]
  ;; handle both integer input and strings that parse as integers
  (if-let [parsed-seconds (core/safe-parse-int duration)]
    parsed-seconds
    ;; must be a format string
    (let [matches (re-seq matching-pattern duration)]
      (if-let [last-matches (last matches)] ;; in the form [<matched string> <first-group> <second-group>]
        (* (core/parse-int (second last-matches)) (get unit-map (last last-matches)))
        (throw (IllegalArgumentException. (format "Invalid duration format %s" duration)))))))

(defn duration-str->Duration
  "Given a puppet duration string, return a java.time.Duration equivalent"
  ^Duration [duration]
  (Duration/ofSeconds (duration-str->seconds duration)))