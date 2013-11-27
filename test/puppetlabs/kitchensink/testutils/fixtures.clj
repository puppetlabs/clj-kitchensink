(ns puppetlabs.kitchensink.testutils.fixtures
  (:require [puppetlabs.kitchensink.core :as kitchensink]))

(defn with-no-jvm-shutdown-hooks
  "Test fixture to prevent JVM shutdown hooks from being added.
  Only works if the shutdown hook is being added by a call to the
  utility function `puppetlabs.kitchensink.core/add-shutdown-hook!`."
  [f]
  (with-redefs [kitchensink/add-shutdown-hook! (fn [_] nil)]
    (f)))
