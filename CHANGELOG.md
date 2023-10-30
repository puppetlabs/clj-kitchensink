## unreleased
* change use of java.security.cert.X509Certificate/getSubjectDN, which is now deprecated, to java.security.cert.X509Certificate/getSubjectX500Principal
* add function `get-lein-project-version` to retrieve a given project version from the standard `lein` system properties.
* mark deprecated interfaces with metadata deprecation
* add 'unzip-file' routine to the files namespace to decompress zipped files
* add 'untar-file' routine to the files namespace to decompress tar files
* update clj-parent to 5.6.6
* remove reflection use in atomic-write

## 3.2.3
* add clj-kondo linting, eastwood linting, and PR testing
* address issues identified by clj-kondo, and eastwood
* add function equivalent of puppet's versioncmp function, and helper functions for to determine if strings have all numeric characters and if strings start with a leading zero.

## 3.2.2

* add conversion functions from string -> ZonedDateTime -> string to facilitate the removal of clj-time
* update clj-parent to 5.3.7

## 3.2.1

* Update ini4j to 0.5.4 to address a Denial of Service
  vulnerability (CVE-2022-41404).

## 3.2.0

This is a minor feature release.

* Add key->str function to convert keywords to strings

## 3.1.3

* Add base-type function that returns the base type from an HTTP Content-Type header.

## 3.1.2

* Update clj-parent to resolve some outdated dependencies with security issues.

## 3.1.1

* Don't select an open port from the ephemeral range in open-port-num.

## 3.1.0

This is a minor feature release.

* Add a function for atomic file writes

## 3.0.0

Maintenance:
* Update dependencies to take up clj-parent 4

## 2.5.2

This is a minor maintenance release.

Maintenance:
* Fix adding URLs to classpath under Java 9.

## 2.5.1

This is a minor maintenance release.

Maintenance:
* Fix symbol redef warnings under Clojure 1.9

## 2.5.0

This is a minor feature release.

Features:
* add a `stream->sha256` function for hashing the contents of an InputStream

## 2.4.0

This is a minor feature and improvement release.

Features:
* add a `utf8-string->sha256` function, directly analogous to `utf8-string->sha1`
* add a `file->sha256` function, equivalent to reading a file's contents as a
  UTF-8 string and hashing the result. Uses an InputStream internally to avoid
  reading the entire file into memory at once.

Improvement:
* the `open-port-num` function should now return a random port number from the
  entire traditional ephemeral port range of 49152 through 65535.

## 2.3.0

This is a minor feature release.

Features:
* add a parser for period strings (7d, 12h, etc) into Joda Periods
* fix file connection leaks in the functions 'lines' and 'ini-to-map'

## 2.2.0

This is a minor feature release.

Features:

* Add an `assoc-if-new` macro, which associates a map key to a value only if
  the key does not already exist in the map.
* Add a `deref-swap!` function, which behaves like deref but returns the old
  value instead of the new one.
* Add a `rand-str` function, for generating random strings from various
  character sets.

Maintenance:

* Update to dynapath 0.2.5, to address some compatibility issues with Java 9.

## 2.1.1

The 2.1.1 release was burned and folded into 2.2.0.

## 2.1.0

This is a minor feature release.

Features:

* Add an `open-port-num` function, which returns a currently open port. Tests
  that bind services to ports can use this to guard against chance port
  collisions.

## 2.0.0

This is a bugfix release which contains one backward incompatible change.

Bug fix:

* Changes all of the maps in various slingshot errors thrown to use `:kind` and `:msg`
  in place of `:type` and `:message`, respectively.

## 1.4.0

This is a minor feature release, which also includes some bugfixes and maintenance work.

Features:

* Add `uuid?` predicate function for determining whether a string is a valid UUID.

Bug fixes:

* Fix an issue in `with-additional-classpath-entries` wherein it's pre/post-conditions were
  not handling arguments properly.

Maintenance:
* Reduce use of reflection
* Remove unused plugins and jenkins scripts
* Switch to `lein-parent` for managing dependency versions

## 1.3.1

This is a maintenance release.

* Remove retired :flag option from cli tooling, to eliminate warnings on CLI
  invocations.
* Bump to org.clojure/tools.cli 0.3.3.

## 1.3.0

This is a maintenance / minor feature release.

* [TK-315](https://tickets.puppetlabs.com/browse/TK-315) - update to latest version
  of `raynes.fs` to reduce downstream dependency conflicts.
* Add an `absolute-path` fn to replace the one that was removed from raynes.fs
* Add a `normalized-path` fn to replace the one that was removed from raynes.fs

## 1.2.0

* Add `temp-file-name` function, which returns a unique name to a temporary file,
  but does not actually create the file.
* Add `with-timeout` macro, which returns a default value if executing an
  arbitrary block of code takes longer than a specified timeout.

## 1.1.0

* Add new `walk-leaves` function for applying a function to all of the leaf
  nodes of a map
* Add new `zipper?` predicate which can be used to assert that an object
  is a clojure zipper.
* Add new `while-let` macro
* Add new `rand-weighted-selection` function
* Add new `to-sentence` variant of string join

## 1.0.0
* Promoting previous release to 1.0.0 so that we can be more deliberate about
  adhering to semver from now on.

## 0.7.3
* Add 'filter-map' function that can be used to filter maps

## 0.7.2
* Change `mkdirs!` to allow string as path arg (7097bb3)
* Add a new `dissoc-in` function, for removing data from nested maps.

## 0.7.1
* Add a new `to-bool` function, which provides a more tolerant way to coerce
  data to booleans

## 0.7.0
* Upgrade fs dependency to 1.4.5 (to standardize across projects)
* Add mkdirs! function to create parent directories with better failure reporting
* Move temp file functions from testutils to core

## 0.6.0
* Remove SSL utility code, which is now available in [puppetlabs/certificate-authority](https://github.com/puppetlabs/jvm-certificate-authority).

## 0.5.4
* Upgrade cheshire dependency to version 5.3.1.

## 0.5.3
* .ini parsing utilities now throw an Exception if a key appears in the file(s) more than once.
* Added a `with-no-jvm-shutdown-hooks` macro for running a block of code without any JVM shutdown hooks.

## 0.5.2
 * Minor change to the cli! function so that, in addition to the data that it already returned, it now also returns a string representation of a banner/usage summary.  Callers can use this to display a help message if additional validation of the cli args fails.
 * Utility functions added to ssl namespace that allow creation of an SSLContext or a KeyStore/TrustStore directly from the pem files.
 * Added some JSON utility functions
 * Added a deep-merge utility function
