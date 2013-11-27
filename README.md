# puppetlabs/kitchensink

A library of utility functions that are common to several Puppet Labs
clojure projects.

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
