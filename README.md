# Omnitool

[![Build Status](https://travis-ci.org/CommBank/omnitool.svg?branch=master)](https://travis-ci.org/CommBank/omnitool)
[![Gitter chat](https://badges.gitter.im/CommBank.png)](https://gitter.im/CommBank)

[Scaladoc](https://commbank.github.io/omnitool/latest/api/index.html)

Usage
-----

See https://commbank.github.io/omnitool

Core
----

`omnitool-core` provides the `Result` ADT and the `ResultantMonad` for dealing with operations that
might fail.

It also has utility functions for:

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
* `DateFormatInfo` - query string parsers for information that is not easily available through public API.
* `DateOrder` - ordering for different time classes.


Log
---

`omnitool-log` provides some convienience methods on [log4j](http://logging.apache.org/log4j/1.2/) `Loggers`.

