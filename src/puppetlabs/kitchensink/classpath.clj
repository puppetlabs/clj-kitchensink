(ns puppetlabs.kitchensink.classpath
  (:import (java.net URLClassLoader URL))
  (:require [clojure.java.io :refer [file Coercions]]))

(defn jar-or-dir-to-url
  "Given the path to a jar file or a directory, return a `java.net.URL`
  object suitable for using with a `URLClassLoader`"
  [jar-or-dir]
  {:pre [(satisfies? Coercions jar-or-dir)]
   :post [(instance? URL %)]}
  ;; explicitly calling `getAbsoluteFile` causes relative file paths to
  ;; be evaluated relative to the system property `user.dir` (which is
  ;; usually set to the current working directory).  This is useful for
  ;; tests and other code that wants to emulated changing the working
  ;; directory.
  (.. (file jar-or-dir) getAbsoluteFile toURL))

(defmacro with-additional-classpath-entries
  "This macro takes a list of paths as an argument.  It then temporarily
  overrides the classpath to include the specified paths; the original
  classpath is restored prior to returning."
  [jars-and-dirs & body]
  {:pre [(coll? jars-and-dirs)
         (every? (partial satisfies? Coercions) jars-and-dirs)]}
  `(let [orig-loader# (.. Thread currentThread getContextClassLoader)
         temp-loader# (URLClassLoader.
                        (into-array
                          URL
                          (map jar-or-dir-to-url ~jars-and-dirs))
                        orig-loader#)]
     (try
       (.. Thread currentThread (setContextClassLoader temp-loader#))
       ~@body
       (finally (.. Thread currentThread (setContextClassLoader orig-loader#))))))
