;; ## "The Kitchen Sink"
;;
;; Pretty much everything in here should _probably_ be organized into
;; proper namespaces, or perhaps even separate libraries
;; altogether. But who has time for that?

(ns puppetlabs.kitchensink.core
  (:import [org.ini4j Ini Config]
           [javax.naming.ldap LdapName]
           [java.io StringWriter Reader IOException File])
  (:require [clojure.test]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [digest]
            [slingshot.slingshot :refer [throw+]]
            [me.raynes.fs :as fs])
  (:use [clojure.java.io :only (reader)]
        [clojure.set :only (difference union)]
        [clojure.string :only (split)]
        [clojure.stacktrace :only (print-cause-trace)]
        [clojure.pprint :only [pprint]]
        [clj-time.core :only [now]]
        [clj-time.coerce :only [ICoerce to-date-time]]
        [clj-time.format :only [formatters unparse]]))

;; ## Type checking

(defn array?
  "Returns true if `x` is an array"
  [x]
  (some-> x
    (class)
    (.isArray)))

(defn datetime?
  "Predicate returning whether or not the supplied object is
  convertible to a Joda DateTime"
  [x]
  (and
    (satisfies? ICoerce x)
    (to-date-time x)))

(defn boolean?
  "Returns true if the value is a boolean"
  [value]
  (instance? Boolean value))

(defn regexp?
  "Returns true if the type is a regexp pattern"
  [regexp]
  {:post [(boolean? %)]}
  (instance? java.util.regex.Pattern regexp))

;; ## String utilities

(defn strict-parse-bool
  "Parse a string and return its boolean value; throws an exception if the String
  does not match `\"true\"` or `\"false\"` (case-insensitive)."
  [s]
  {:pre [(string? s)]
   :post [(boolean? %)]}
  (condp = (.toLowerCase s)
    "true" true
    "false" false
    (throw+ {:type ::parse-error
             :message (format "Unable to parse '%s' to a boolean" s)})))

(defn parse-bool
  "Parse a string and return its boolean value."
  [s]
  {:pre [(or (nil? s) (string? s))]
   :post [(boolean? %)]}
  (Boolean/parseBoolean s))

(defn to-bool
  "Converts the argument to a boolean.  The behavior is as follows:

   * If the argument is a Boolean, it is simply returned.
   * If the argument is a String, returns the Boolean `true` if the String
     matches `\"true\"` (case insensitive), or `false` if the String matches
     `\"false\"` (case insensitive).  Throws an exception otherwise.
   * If the argument is `nil`, returns false."
  [val]
  {:pre [((some-fn boolean? string? nil?) val)]
   :post [(boolean? %)]}
  (cond
    (boolean? val) val
    (string? val) (strict-parse-bool val)
    (nil? val) false))

(defn string-contains?
  "Returns true if `s` has the `substring` in it"
  [substring s]
  {:pre [(string? s)
         (string? substring)]}
  (>= (.indexOf s substring) 0))

(defn true-str?
  "Return true if the string contains true"
  [^String s]
  (.equalsIgnoreCase "true" s))

(defn pprint-to-string [x]
  (let [w (StringWriter.)]
    (pprint x w)
    (.toString w)))

;; ## I/O

(defn lines
  "Returns a sequence of lines from the given filename"
  [filename]
  (-> filename
    (fs/file)
    (reader)
    (line-seq)))

(defn mkdirs!
  "Given a path (may be a File or a string), creates a directory (including any
  missing parent directories).  Throws a slingshot exception with a meaningful
  error message if the directory cannot be created.

  (The reason for the existence of this function is that the Java File.mkdirs
  method only returns a boolean indicating whether the directory was created;
  if you get back a `false`, you have no idea whether it failed due to permission
  errors, or the path being invalid in some way, or the directory already exists.)

  The slingshot exception will look like this:

  `{:type     :puppetlabs.kitchensink.core/io-error
    :message  \"Parent directory '/foo/bar' is not writable\"}`"
  [path]
  {:pre [((some-fn #(instance? File %) string?) path)]
   :post [(fs/directory? path)]}
  (let [path-as-file (fs/file path)]
    (if (fs/file? path-as-file)
      (throw+ {:type    ::io-error
               :message (format "Path '%s' is a file" path)})
      (doseq [dir (reverse (cons path-as-file (fs/parents path-as-file)))]
        (when-not (fs/exists? dir)
          (let [parent (.getParentFile dir)]
            (when (fs/file? parent)
              (throw+ {:type ::io-error
                       :message (format "Parent directory '%s' is a file"
                                        parent)}))

            (when-not (.canWrite parent)
              (throw+ {:type ::io-error
                       :message (format "Parent directory '%s' is not writable"
                                        parent)}))

            (let [success (.mkdir dir)]
              (when-not success
                (throw+ {:type ::io-error
                         :message (format "Unable to create directory '%s'"
                                          parent)})))))))))

;; ## Math

(defn quotient
  "Performs division on the supplied arguments, substituting `default`
  when the divisor is 0"
  ([dividend divisor]
    (quotient dividend divisor 0))
  ([dividend divisor default]
    (if (zero? divisor)
      default
      (/ dividend divisor))))

;; ## Numerics

(defn parse-int
  "Parse a string `s` as an integer, returning nil if the string doesn't
  contain an integer."
  [s]
  {:pre  [(string? s)]
   :post [(or (integer? %) (nil? %))]}
  (try (Integer/parseInt s)
    (catch java.lang.NumberFormatException e
      nil)))

(defn parse-float
  "Parse a string `s` as a float, returning nil if the string doesn't
  contain a float"
  [s]
  {:pre  [(string? s)]
   :post [(or (float? %) (nil? %))]}
  (try (Float/parseFloat s)
    (catch java.lang.NumberFormatException e
      nil)))

(defn parse-number
  "Converts a string `s` to a number, by attempting to parse it as an integer
  and then as a float. Returns nil if the string isn't numeric."
  [s]
  {:pre  [(string? s)]
   :post [(or (number? %) (nil? %))]}
  ((some-fn parse-int parse-float) s))


;; ## Collection operations

(defn symmetric-difference
  "Computes the symmetric difference between 2 sets"
  [s1 s2]
  (union (difference s1 s2) (difference s2 s1)))

(defn as-collection
  "Returns the item wrapped in a collection, if it's not one
  already. Returns a list by default, or you can use a constructor func
  as the second arg."
  ([item]
    (as-collection item list))
  ([item constructor]
    {:post [(coll? %)]}
    (if (coll? item)
      item
      (constructor item))))

(defn seq-contains?
  "True if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn enumerate
  "Returns a lazy sequence consisting of 0 and the first item of coll,
  followed by 1 and the second item in coll, etc, until coll is
  exhausted."
  [coll]
  (map-indexed vector coll))

(def excludes?
  "Inverse of `contains?`.  Returns false if key is present in the given collectoin,
  otherwise returns true."
  (complement contains?))

(defn contains-some
  "If coll `contains?` any of the keys in ks, returns the first such
  key.  Otherwise returns nil."
  [coll ks]
  (some #(if (contains? coll %) %) ks))

(defn excludes-some
  "If coll `excludes?` any of the keys in ks, returns the first such
  key.  Otherwise returns nil."
  [coll ks]
  (some #(if (excludes? coll %) %) ks))

(defn mapvals
  "Return map `m`, with each value transformed by function `f`.

  You may also provide an optional list of keys `ks`; if provided, only the
  specified keys will be modified."
  ([f m]
    (into {} (for [[k v] m] [k (f v)])))
  ([f ks m]
    ;; would prefer to share code between the two implementations here, but
    ;; the `into` is much faster for the base case and the reduce is much
    ;; faster for any case where we're operating on a subset of the keys.
    ;; It seems like `select-keys` is fairly expensive.
    (reduce (fn [m k] (update-in m [k] f)) m ks)))

(defn mapkeys
  "Return map `m`, with each key transformed by function `f`"
  [f m]
  (into {} (concat (for [[k v] m]
                     [(f k) v]))))

(defn maptrans
  "Return map `m`, with values transformed according to the key-to-function
  mappings specified in `keys-fns`.  `keys-fns` should be a map whose keys
  are lists of keys from `m`, and whose values are functions to apply to those
  keys.

  Example: `(maptrans {[:a, :b] inc [:c] dec} {:a 1 :b 1 :c 1})` yields `{:a 2, :c 0, :b 2}`"
  [keys-fns m]
  {:pre [(map? keys-fns)
         (every? (fn [[ks fn]] (and (coll? ks) (ifn? fn))) keys-fns)
         (map? m)]}
  (let [ks (keys keys-fns)]
    (reduce (fn [m k] (mapvals (keys-fns k) k m)) m ks)))

(defn dissoc-if-nil
  "Given a map and a key, checks to see if the value for the key is `nil`; if so,
  returns a modified map with the specified key removed.  If the value is not `nil`,
  simply returns the original map."
  ([m k]
    {:pre  [(map? m)]
     :post [(map? %)]}
    (if (nil? (m k))
      (dissoc m k)
      m))
  ([m k & ks]
    (let [ret (dissoc-if-nil m k)]
      (if ks
        (recur ret (first ks) (next ks))
        ret))))

(defn dissoc-in
  "Dissociates an entry from a nested map. ks is a sequence of keys. Any empty maps that result
  will not be present in the new map."
  [m [k & ks]]
  (when m
    (if-let [res (and ks (dissoc-in (m k) ks))]
      (assoc m k res)
      (let [res (dissoc m k)]
        (when-not (empty? res)
          res)))))

(defn merge-with-key
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key `k` occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f k val-in-result val-in-latter)."
  {:added "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f k (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn deep-merge
  "Deeply merges maps so that nested maps are combined rather than replaced.

  For example:
  (deep-merge {:foo {:bar :baz}} {:foo {:fuzz :buzz}})
  ;;=> {:foo {:bar :baz, :fuzz :buzz}}

  ;; contrast with clojure.core/merge
  (merge {:foo {:bar :baz}} {:foo {:fuzz :buzz}})
  ;;=> {:foo {:fuzz :quzz}} ; note how last value for :foo wins"
  [& vs]
  (if (every? map? vs)
    (apply merge-with deep-merge vs)
    (last vs)))

(defn deep-merge-with
  "Deeply merges like `deep-merge`, but uses `f` to produce a value from the
  conflicting values for a key in multiple maps."
  [f & vs]
  (if (every? map? vs)
    (apply merge-with (partial deep-merge-with f) vs)
    (apply f vs)))

(defn deep-merge-with-keys*
  "Helper function for deep-merge-with-keys"
  [f ks & vs]
  (if (every? map? vs)
    (apply merge-with-key
           (fn [k & vs] (apply deep-merge-with-keys* f (conj ks k) vs))
           vs)
    (apply f ks vs)))

(defn deep-merge-with-keys
  "Deeply merges like `deep-merge`, but uses `f` to produce a value from the
  conflicting values for a key path `ks` that appears in multiple maps, by calling
  `(f ks val-in-result val-in-latter)`."
  [f & vs]
  (apply deep-merge-with-keys* f [] vs))

(defn keyset
  "Returns the set of keys from the supplied map"
  [m]
  {:pre  [(map? m)]
   :post [(set? %)]}
  (set (keys m)))

(defn valset
  "Returns the set of values from the supplied map"
  [m]
  {:pre  [(map? m)]
   :post [(set? %)]}
  (set (vals m)))

(def select-values
  "Returns the sequence of values from the map for the entries with the specified keys"
  (comp vals select-keys))

(defn missing?
  "Inverse of contains? that supports multiple keys. Will return true if all items are
  missing from the collection, false otherwise.

  Example:

      ;; Returns true, as :z :f :h are all missing
      (missing? {:a 'a' :b 'b' :c 'c'} :z :f :h)

      ;; Returns false, as :a is in the collection
      (missing? {:a 'a' :b 'b' :c 'c'} :z :b)"
  [coll & keys]
  {:pre  [(coll? coll)]
   :post [(boolean? %)]}
  (reduce (fn [_ key]
            (if (contains? coll key)
              (reduced false)
              true))
    nil
    keys))

(defn ordered-comparator
  "Given a function and an order (:ascending or :descending),
  return a comparator function that takes two objects and compares them in
  ascending or descending order based on the value of applying the function
  to each."
  [f order]
  {:pre  [(ifn? f)
          (contains? #{:ascending :descending} order)]
   :post [(fn? %)]}
  (fn [x y]
    (if (= order :ascending)
      (compare (f x) (f y))
      (compare (f y) (f x)))))

(defn compose-comparators
  "Composes two comparator functions into a single comparator function
  which will call the first comparator and return the result if it is
  non-zero; otherwise it will call the second comparator and return
  its result."
  [comp-fn1 comp-fn2]
  {:pre  [(fn? comp-fn1)
          (fn? comp-fn2)]
   :post [(fn? %)]}
  (fn [x y]
    (let [val1 (comp-fn1 x y)]
      (if (= val1 0)
        (comp-fn2 x y)
        val1))))

(defn order-by-expr?
  "Predicate that returns true if the argument is a valid expression for use
  with the `order-by` function; in other words, returns true if the argument
  is a 2-item vector whose first element is an `ifn` and whose second element
  is either `:ascending` or `:descending`."
  [x]
  (and
    (vector? x)
    (ifn? (first x))
    (contains? #{:ascending :descending} (second x))))

(defn order-by
  "Sorts a collection based on a sequence of 'order by' expressions.  Each expression
  is a tuple containing a fn followed by either `:ascending` or `:descending`;
  returns a collection that is sorted based on the values of the 'order by' fns
  being applied to the elements in the original collection.  If multiple 'order by'
  expressions are passed in, their precedence is determined by their order in
  the argument list."
  [order-bys coll]
  {:pre [(sequential? order-bys)
         (every? order-by-expr? order-bys)
         (coll? coll)]}
  (let [comp-fns    (map (fn [[f order]] (ordered-comparator f order)) order-bys)
        final-comp  (reduce compose-comparators comp-fns)]
    (sort final-comp coll)))

(defn sort-nested-maps
  "For a data structure, recursively sort any nested maps and sets descending
  into map values, lists, vectors and set members as well. The result should be
  that all maps in the data structure become explicitly sorted with natural
  ordering. This can be used before serialization to ensure predictable
  serialization.

  The returned data structure is not a transient so it is still able to be
  modified, therefore caution should be taken to avoid modification else the
  data will lose its sorted status."
  [data]
  (cond
    (map? data)
    (into (sorted-map) (for [[k v] data]
                         [k (sort-nested-maps v)]))
    (sequential? data)
    (map sort-nested-maps data)
    :else data))

;; ## Date and Time

(defn timestamp
  "Returns a timestamp string for the given `time`, or the current time if none
  is provided. The format of the timestamp is eg. 2012-02-23T22:01:39.539Z."
  ([]
    (timestamp (now)))
  ([time]
    (unparse (formatters :date-time) time)))

;; ## Exception handling

(defn without-ns
  "Given a clojure keyword that is optionally namespaced, returns
  a keyword with the same name but with no namespace."
  [kw]
  {:pre [(keyword? kw)]
   :post [(keyword? %)
          (nil? (namespace %))]}
  (keyword (name kw)))

(defn keep-going*
  "Executes the supplied fn repeatedly. Execution may be stopped with an
  InterruptedException."
  [f on-error]
  (if (try
        (f)
        true
        (catch InterruptedException e
          false)
        (catch Throwable e
          (on-error e)
          true))
    (recur f on-error)))

(defmacro keep-going
  "Executes body, repeating the execution of body even if an exception
  is thrown"
  [on-error & body]
  `(keep-going* (fn [] ~@body) ~on-error))

(defmacro with-error-delivery
  "Executes body, and delivers an exception to the provided promise if one is
  thrown."
  [error & body]
  `(try
     ~@body
     (catch Throwable e#
       (deliver ~error e#))))

;; ## Temp files

(defn delete-on-exit
  "Will delete `f` on shutdown of the JVM"
  [f]
  (.deleteOnExit (fs/file f))
  f)

(defn temp-file
  "Creates a temporary file that will be deleted on JVM shutdown.

  Supported arguments are the same as for me.raynes.fs/temp-file:
  [prefix]
  [prefix suffix]
  [prefix suffix tries]

  You may also call with no arguments, in which case the prefix string will be
  empty."
  [& args]
  (if (empty? args)
    (delete-on-exit (fs/temp-file nil))
    (delete-on-exit (apply fs/temp-file args))))

(defn temp-dir
  "Creates a temporary directory that will be deleted on JVM shutdown.

  Supported arguments are the same as for me.raynes.fs/temp-dir:
  [prefix]
  [prefix suffix]
  [prefix suffix tries]

  You may also call with no arguments, in which case the prefix string will be
  empty."
  [& args]
  temp-dir
  (if (empty? args)
    (delete-on-exit (fs/temp-dir nil))
    (delete-on-exit (apply fs/temp-dir args))))

;; ## Configuration files

(def keywordize
  "Normalize INI keys by ensuring they're lower-case and keywords"
  (comp keyword string/lower-case))

(defn fetch-int
  "Fetch a key from the INI section and convert it
   to an integer if it parses, otherwise return the string"
  [section key]
  (let [val (.fetch section key)]
    (or (parse-int val)
        val)))

(defn create-section-map
  "Given an INI section, create a clojure map of it's key/values"
  [section]
  (reduce (fn [acc [key _]]
            (if (> (.length section key) 1)
              (throw (IllegalArgumentException.
                       (str "Duplicate configuration entry: "
                            (mapv keyword [(.getName section) key]))))
              (assoc acc
                (keywordize key)
                (fetch-int section key))))
          {} section))

(defn parse-ini
  "Takes a reader that contains an ini file, and returns an Ini object
  containing the parsed results"
  [ini-reader]
  {:pre [(instance? Reader ini-reader)]
   :post [(instance? Ini %)]}
  (let [config (Config.)
        ini (Ini.)]
    (.setMultiOption config true)
    (.setConfig ini config)
    (.load ini ini-reader)
    ini))

(defn ini-to-map
  "Takes a .ini filename and returns a nested map of
  fully-interpolated values. Strings that look like integers are
  returned as integers, and all section names and keys are returned as
  symbols."
  [filename]
  {:pre  [(or (string? filename)
              (instance? java.io.File filename))]
   :post [(map? %)
          (every? keyword? (keys %))
          (every? map? (vals %))]}

  (reduce (fn [acc [name section]]
            (assoc acc
              (keywordize name)
              (create-section-map section)))
          {}
          (parse-ini (reader filename))))

(defn inis-to-map
  "Takes a path and converts the pointed-at .ini files into a nested
  map (see `ini-to-map` for details). If `path` is a file, the
  behavior is exactly the same as `ini-to-map`. If `path` is a
  directory, we return a merged version of parsing all the .ini files
  in the directory (we do not do a recursive find of .ini files)."
  ([path]
    (inis-to-map path "*.ini"))
  ([path glob-pattern]
    {:pre  [(or (string? path)
              (instance? java.io.File path))]
     :post [(map? %)]}
    (let [files (if-not (fs/directory? path)
                  [path]
                  (fs/glob (fs/file path glob-pattern)))]
      (->> files
        (map fs/absolute-path)
        (map ini-to-map)
        (apply deep-merge-with-keys
               (fn [ks & _]
                 (throw (IllegalArgumentException.
                          (str "Duplicate configuration entry: " ks)))))
        (merge {})))))

(defn spit-ini
  "Writes the `ini-map` to the Ini file at `file`. `ini-map` should
   a map similar to the ones created by ini-to-map. The keys are keywords
   for the sections and their values are maps of config keypairs."
  [file ini-map]
  (let [ini (org.ini4j.Ini. file)]
    (doseq [[section-key section] ini-map
            [k v] section]
      (.put ini (name section-key) (name k) v))
    (.store ini)))

(defn add-shutdown-hook!
  "Adds a shutdown hook to the JVM runtime.

  `f` is a function that takes 0 arguments; the return value is ignored.  This
  function will be called if the JVM receiveds an interrupt signal (e.g. from
  `kill` or CTRL-C); you can use it to log shutdown messages, handle state
  cleanup, etc."
  [f]
  {:pre [(fn? f)]}
  (.addShutdownHook (Runtime/getRuntime) (Thread. f)))

(defmacro demarcate
  "Executes `body`, but logs `msg` to info before and after `body` is
  executed. `body` is executed in an implicit do, and the last
  expression's return value is returned by `demarcate`.

    user> (demarcate \"reticulating splines\" (+ 1 2 3))
    \"Starting reticulating splines\"
    \"Finished reticulating splines\"
    6
  "
  [msg & body]
  `(do (log/info (str "Starting " ~msg))
     (let [result# (do ~@body)]
       (log/info (str "Finished " ~msg))
       result#)))

;; ## Command-line parsing

(defn cli!
  "Validates that required command-line arguments are present. If they are not,
  throws a map** with an error message that is intended to be displayed to the user.
  Also checks to see whether the user has passed the `--help` flag.

  Input:

  - args     : the command line arguments passed in by the user
  - specs    : an array of supported argument specifications, as accepted by
               `clojure.tools.cli`
  - required : an array of keywords (using the long form of the argument spec)
               specifying which of the `specs` are required.  If any of the
               `required` options are not present, the function will cause
               the program to exit and display the help message.

  ** The map is thrown using 'slingshot' (https://github.com/scgilardi/slingshot).
  It contains a `:type` and `:message`, where type is either `:error` or `:help`,
  and the message is either the error message or a help banner.

  Returns a three-item vector, containing:
  * a map of the parsed options
  * a vector containing the remaining cli arguments that were not parsed
  * a string containing a summary of all of the options that are available; for
    use in printing help messages if the user detects that the arguments are
    still invalid in some way."
  ([args specs] (cli! args specs nil))
  ([args specs required-args]
  (let [specs (conj specs ["-h" "--help" "Show help" :default false :flag true])
        {:keys [options arguments summary errors]} (cli/parse-opts args specs)]
    (when errors
      (let [msg (str
                  "\n\n"
                  "Error(s) occurred while parsing command-line arguments: "
                  (apply str errors)
                  "\n\n"
                  summary)]
        (throw+ {:type ::cli-error
                 :message msg})))
    (when (:help options)
      (throw+ {:type ::cli-help
               :message summary}))
    (when-let [missing-field (some #(if (not (contains? options %)) %) required-args)]
      (let [msg (str
                  "\n\n"
                  (format "Missing required argument '--%s'!" (name missing-field))
                  "\n\n"
                  summary)]
        (throw+ {:type ::cli-error
                 :message msg})))
    [options arguments summary])))


;; ## SSL Certificate handling
;;
;; NOTE: Prefer functions provided by the jvm-certificate-authority library over these.
;;
;; These functions are only used by PuppetDB and they should likely move back into that
;; project until they can be refactored away over functions from the jvm-ca library.

(defn cn-for-dn
  "Deprecated. Use functions from `jvm-certificate-authority`.

  Extracts the CN (common name) from an LDAP DN (distinguished name).

  If more than one CN entry exists in the given DN, we return the most-specific
  one (the one that comes last, textually). If no CN is present in the DN, we
  return nil.

  Example:

      (cn-for-dn \"CN=foo.bar.com,OU=meh,C=us\")
      \"foo.bar.com\"

      (cn-for-dn \"CN=foo.bar.com,CN=baz.goo.com,OU=meh,C=us\")
      \"baz.goo.com\"

      (cn-for-dn \"OU=meh,C=us\")
      nil"
  [dn]
  {:pre [(string? dn)]}
  (some->> dn
    (LdapName.)
    (.getRdns)
    (filter #(= "CN" (.getType %)))
    (first)
    (.getValue)
    (str)))

(defn cn-for-cert
  "Deprecated. Use functions from `jvm-certificate-authority`.

  Extract the CN from the DN of an x509 certificate. See `cn-for-dn` for details
  on how extraction is performed.

  If no CN exists in the certificate DN, nil is returned."
  [^java.security.cert.X509Certificate cert]
  (-> cert
    (.getSubjectDN)
    (.getName)
    (cn-for-dn)))

;; ## Ring helpers

(defn cn-whitelist->authorizer
  "Given a 'whitelist' file containing allowed CNs (one per line),
   build a function that takes a Ring request and returns true if the
   CN contained in the client certificate appears in the whitelist.

   `whitelist` can be either a local filename or a File object.

   This makes use of the `:ssl-client-cn` request parameter. See
   `com.puppetlabs.middleware/wrap-with-certificate-cn`."
  [whitelist]
  {:pre  [(or (string? whitelist)
            (instance? java.io.File whitelist))]
   :post [(fn? %)]}
  (let [allowed? (set (lines whitelist))]
    (fn [{:keys [ssl-client-cn scheme] :as req}]
      (or (= scheme :http)
        (allowed? ssl-client-cn)))))

;; ## Hashing

(defn utf8-string->sha1
  "Compute a SHA-1 hash for the UTF-8 encoded version of the supplied
  string"
  [s]
  {:pre  [(string? s)]
   :post [(string? %)]}
  (let [bytes (.getBytes s "UTF-8")]
    (digest/sha-1 [bytes])))

(defn bounded-memoize
  "Similar to memoize, but the cache will be reset if the number of entries
  exceeds the specified `bound`."
  [f bound]
  {:pre [(integer? bound)
         (pos? bound)]}
  (let [cache (atom {})]
    (fn [& args]
      (if-let [e (find @cache args)]
        (val e)
        (let [v (apply f args)]
          (when (> (count @cache) bound)
            (reset! cache {}))
          (swap! cache assoc args v)
          v)))))

;; ## UUID handling

(defn uuid
  "Generate a random UUID and return its string representation"
  []
  (str (java.util.UUID/randomUUID)))

;; ## System interface

(defn num-cpus
  "Grabs the number of available CPUs for the local host"
  []
  {:post [(pos? %)]}
  (.availableProcessors (Runtime/getRuntime)))

;; Comparison of JVM versions

(defn compare-jvm-versions
  "Same behavior as `compare`, but specifically for JVM version
   strings.  Because Java versions don't follow semver or anything, we
   need to do some massaging of the input first:

  http://www.oracle.com/technetwork/java/javase/versioning-naming-139433.html"
  [a b]
  {:pre  [(string? a)
          (string? b)]
   :post [(number? %)]}
  (let [parse #(mapv parse-int (-> %
                                 (split #"-")
                                 (first)
                                 (split #"[\\._]")))]
    (compare (parse a) (parse b))))

(def java-version
  "Returns a string of the currently running java version"
  (System/getProperty "java.version"))

;; control flow

(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))


(defmacro some-pred->>
  "When expr does not satisfy pred, threads it into the first form (via ->>),
  and when that result does not satisfy pred, through the next etc"
  [pred expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(if (~pred ~g) ~g (->> ~g ~step)))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep forms))]
       ~g)))
