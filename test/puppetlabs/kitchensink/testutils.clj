(ns puppetlabs.kitchensink.testutils
  (:require [fs.core :as fs]))

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

(defn delete-on-exit
  "Will delete `f` on shutdown of the JVM"
  [f]
  (.deleteOnExit (fs/file f))
  f)

(def ^{:doc "Creates a temporary file that will be deleted on JVM shutdown."}
  temp-file
  (comp delete-on-exit fs/temp-file))

(def ^{:doc "Creates a temporary directory that will be deleted on JVM shutdown."}
  temp-dir
  (comp delete-on-exit fs/temp-dir))
