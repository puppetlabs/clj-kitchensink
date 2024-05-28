(defproject puppetlabs/kitchensink "3.4.0-SNAPSHOT"
  :description "Clojure utility functions"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}

  :min-lein-version "2.9.1"

  :parent-project {:coords [puppetlabs/clj-parent "5.6.14"]
                   :inherit [:managed-dependencies]}

  ;; Abort when version ranges or version conflicts are detected in
  ;; dependencies. Also supports :warn to simply emit warnings.
  ;; requires lein 2.2.0+.
  :pedantic? :abort

  :dependencies [[org.clojure/clojure]
                 [org.clojure/tools.logging]
                 [org.clojure/tools.cli]

                 [org.apache.commons/commons-compress]
                 [clj-time]
                 [clj-commons/fs]
                 [slingshot]
                 [cheshire]

                 [org.ini4j/ini4j "0.5.4"]
                 [org.tcrawley/dynapath]
                 [digest "1.4.3"]]

  ;; By declaring a classifier here and a corresponding profile below we'll get an additional jar
  ;; during `lein jar` that has all the code in the test/ directory. Downstream projects can then
  ;; depend on this test jar using a :classifier in their :dependencies to reuse the test utility
  ;; code that we have.
  :classifiers [["test" :testutils]]

  :profiles {:testutils {:source-paths ^:replace ["test"]}}

  ;; this plugin is used by jenkins jobs to interrogate the project version
  :plugins [[lein-project-version "0.1.0"]
            [jonase/eastwood "1.2.2" :exclusions [org.clojure/clojure]]
            [lein-parent "0.3.7"]]

  :eastwood {:ignored-faults {:unused-ret-vals {puppetlabs.kitchensink.classpath [{:line 93}]}
                              :deprecations {puppetlabs.kitchensink.classpath [{:line 66}
                                                                               {:line 91}]
                                             puppetlabs.kitchensink.core true
                                             puppetlabs.kitchensink.core-test true}
                              :reflection {puppetlabs.kitchensink.file [{:line 62}]
                                           puppetlabs.kitchensink.core [{:line 929}]}
                              :constant-test {puppetlabs.kitchensink.core-test [{:line 726}
                                                                                {:line 733}]}}
             :continue-on-exception true}

  :test-selectors {:default (complement :slow)
                   :slow :slow}

  :deploy-repositories [["releases" {:url "https://clojars.org/repo"
                                     :username :env/clojars_jenkins_username
                                     :password :env/clojars_jenkins_password
                                     :sign-releases false}]
                        ["snapshots" "http://nexus.delivery.puppetlabs.net/content/repositories/snapshots/"]])
