# Omnitool

[![Build Status](https://travis-ci.org/CommBank/omnitool.svg?branch=master)](https://travis-ci.org/CommBank/omnitool)
[![Gitter chat](https://badges.gitter.im/CommBank.png)](https://gitter.im/CommBank)

[Scaladoc](https://commbank.github.io/omnitool/latest/api/index.html)

Usage
-----

See https://commbank.github.com.io/omnitool

Core
----

Utility functions for:

* Dealing with errors `Validated`
* Dealing with releasing resources `Closeable`


Time
----

`omnitool-time` provides a thin layer of convenience functions on top of
[joda-time](http://www.joda.org/joda-time/).

* `TimePoint` - wrapper around millisecond since epoch with time related convenience functions
   and parsing code. Use this instead of `DateTime`time if you need to serialise a time value.
* `TimeParser` - safe parses from string to `DateTime`.
* `DateFormat` - default string patters for time such as `yyyy-MM-dd`.
* `DateOrder` - ordering for different time classes.
