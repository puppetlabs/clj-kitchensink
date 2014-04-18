(ns puppetlabs.kitchensink.testutils
  (:require [me.raynes.fs :as fs]
            [puppetlabs.kitchensink.core :as ks]))

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

(defn temp-file
  "Creates a temporary file that will be deleted on JVM shutdown."
  [& args]
  (if (empty? args)
    (delete-on-exit (fs/temp-file nil))
    (delete-on-exit (apply fs/temp-file args))))

(defn temp-dir
  "Creates a temporary directory that will be deleted on JVM shutdown."
  [& args]
  temp-dir
  (if (empty? args)
    (delete-on-exit (fs/temp-dir nil))
    (delete-on-exit (apply fs/temp-dir args))))

(defmacro with-no-jvm-shutdown-hooks
  [& body]
  `(with-redefs [ks/add-shutdown-hook! (fn [_#] nil)]
    ~@body))
