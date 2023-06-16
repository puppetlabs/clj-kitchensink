(ns puppetlabs.kitchensink.testutils
  (:require [puppetlabs.kitchensink.core :as ks]))

(defn call-counter
  "Returns a method that just tracks how many times it's called, and
  with what arguments. That information is stored in metadata for the
  method."
  []
  (let [ncalls    (ref 0)
        arguments (ref [])]
    (with-meta
      (fn [& args]
        (dosync
          (alter ncalls inc)
          (alter arguments conj args)))
      {:ncalls ncalls
       :args   arguments})))

(defn times-called
  "Returns the number of times a `call-counter` function has been
  invoked."
  [f]
  (deref (:ncalls (meta f))))

(defmacro with-no-jvm-shutdown-hooks
  [& body]
  `(with-redefs [ks/add-shutdown-hook! (fn [_#] nil)]
    ~@body))
