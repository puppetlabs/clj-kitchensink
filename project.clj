(defproject puppetlabs/kitchensink "0.2.0-SNAPSHOT"
  :description "Clojure utility functions"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; Logging
                 [org.clojure/tools.logging "0.2.6"]
                 ;; Filesystem utilities
                 [fs "1.1.2"]
                 ;; Configuration file parsing
                 [org.ini4j/ini4j "0.5.2"]
                 [org.clojure/tools.cli "0.2.2"]
                 [digest "1.4.3"]
                 [clj-time "0.5.1"]
                 ;; SSL
                 [org.bouncycastle/bcpkix-jdk15on "1.49"]]

  :profiles {:dev {:resource-paths ["test-resources"]}}

  :deploy-repositories [["snapshots" "http://nexus.delivery.puppetlabs.net/content/repositories/snapshots/"]])
