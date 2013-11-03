(defproject puppetlabs/clj-utils "0.1.0-SNAPSHOT"
  :description "Clojure utility functions"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; Logging
                 [org.clojure/tools.logging "0.2.6"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 ;; Filesystem utilities
                 [fs "1.1.2"]
                 ;; Configuration file parsing
                 [org.ini4j/ini4j "0.5.2"]
                 [org.clojure/tools.cli "0.2.2"]
                 [digest "1.4.3"]
                 [clj-time "0.5.1"]
                 [metrics-clojure "0.7.0" :exclusions [org.clojure/clojure org.slf4j/slf4j-api]]]

  :deploy-repositories [["releases" "http://nexus.delivery.puppetlabs.net/content/repositories/releases/"]
                        ["snapshots" "http://nexus.delivery.puppetlabs.net/content/repositories/snapshots/"]])
