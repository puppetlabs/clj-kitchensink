# puppetlabs/kitchensink

A library of utility functions that are common to several Puppet Labs
clojure projects.

## Installation

Add the following dependency to your `project.clj` file:

[![Clojars Project](http://clojars.org/puppetlabs/kitchensink/latest-version.svg)](http://clojars.org/puppetlabs/kitchensink)

## Using Our Test Utils

Kitchensink provides [utility code](./test/puppetlabs/kitchensink/) for use in tests.
The code is available in a separate "test" jar that you may depend on by using a classifier in your project dependencies.

```clojure
  (defproject yourproject "1.0.0"
    ...
    :profiles {:test {:dependencies [[puppetlabs/kitchensink "x.y.z" :classifier "test"]]}})
```

## License

Copyright © 2013 Puppet Labs

Distributed under the [Apache License, version 2](http://www.apache.org/licenses/).

## Support

Please log tickets and issues at our [Trapperkeeper JIRA tracker](https://tickets.puppetlabs.com/browse/TK).
