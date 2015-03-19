# Puppet Labs: Clojure 'kitchensink' Library

This library contains general-purpose utility code that can be used
across all other apps and projects.

When considering submitting or reviewing contributions to this library,
please try to make sure that the additions are truly general purpose
and likely to be reasonable candidates for re-use in just about any
other project.  Things that are domain-specific for any subset of
other Puppet Labs projects should probably not be included in this
library.

Another consideration: the `core` namespace is prety large an unwieldy
in its current form, and should probably be broken into smaller, more
specific namespaces.  If you are contributing new code and see a
reasonable way to break it off into a different namespace (perhaps moving
some of the existing code from core along to the new namespace with it),
please consider doing so.

# General PL Contribution Guidelines

Third-party patches are essential for keeping puppet open-source projects
great. We want to keep it as easy as possible to contribute changes that
allow you to get the most out of our projects. There are a few guidelines
that we need contributors to follow so that we can have a chance of keeping on
top of things.  For more info, see our canonical guide to contributing:

[https://github.com/puppetlabs/puppet/blob/master/CONTRIBUTING.md](https://github.com/puppetlabs/puppet/blob/master/CONTRIBUTING.md)
