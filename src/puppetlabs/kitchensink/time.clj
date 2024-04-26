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

(defn days->hours
  "Convert a number of days into equivalent hours"
  [days]
  (* 24 days))

(defn hours->days
  "Convert a number of hours into days and fractional days"
  [hours]
  (/ hours 24))

(defn hours->min
  "Convert a number of hours into minutes"
  [hours]
  (* 60 hours))

(defn min->hours
  "Convert a number of minutes into hours and fractional hours"
  [min]
  (/ min 60))

(defn min->sec
  "Convert a number of minutes into seconds"
  [min]
  (* 60 min))

(defn sec->min
  "Convert a number of seconds into minutes and fractional minutes"
  [sec]
  (/ sec 60))

(defn sec->ms
  "Convert a number of seconds into milliseconds"
  [sec]
  (* 1000 sec))

(defn ms->sec
  "Convert a number of milliseconds into seconds and fractional seconds"
  [ms]
  (/ ms 1000))

(defn min->ms
  "Convert a number of minutes into milliseconds"
  [min]
  (-> min
      min->sec
      sec->ms))

(defn ms->min
  "Convert a number of milliseconds into minutes and fractional minutes"
  [ms]
  (-> ms
      ms->sec
      sec->min))

(defn hours->ms
  "Convert a number of hours into milliseconds"
  [hours]
  (-> hours
      hours->min
      min->ms))

(defn ms->hours
  "Convert a number of milliseconds into hours and fractional hours"
  [ms]
  (-> ms
      ms->min
      min->hours))

(defn days->ms
  "Convert a number of days into milliseconds"
  [days]
  (-> days
      days->hours
      hours->ms))

(defn ms->days
  "Convert a number of milliseconds into days and fractional days"
  [ms]
  (-> ms
      ms->hours
      hours->days))