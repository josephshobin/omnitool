//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.omnitool.time

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormatter

import scalaz.\/


/**
  * A `TimePoint` is an encoding of a time with up to millisecond precision.
  *
  * By default this `TimePoint` shall be treated as milliseconds since
  * posix epoch. However, it provides a series of convenience functions for
  * converting to and from joda time primitives at a variety of precisions.
  * It is expected that joda time be used for any actual date manipulation.
  *
  * This data type exists as a simple container format so it can be easily
  * and efficiently transported using java serialization for hadoop (at
  * least it is efficient to the extend that java serialization allows).
  */
case class TimePoint(instant: Long) {
  /** Convert to a DateTime.*/
  def toDateTime: DateTime = new DateTime(instant, DateTimeZone.UTC)

  /** Convert to string using the given formatter.*/
  def toString(format: DateTimeFormatter): String = toDateTime.toString(format)

  /** As string with `yyyy-mm-dd` format.*/
  def day: String = toString(DateFormat.day)

  /** As string with `yyyy-mm` format.*/
  def month: String = toString(DateFormat.month)

  /** As string with `yyyy` format.*/
  def year: String = toString(DateFormat.year)

  /** Map across the long value of TimePoint.*/
  def map(f: Long => Long) = TimePoint(f(instant))

  /** Map across TimePoint as if it was a DateTime.*/
  def mapDateTime(f: DateTime => DateTime) = TimePoint(f(toDateTime))
}

/** A collection of functions to safely create [[TimePoint]].*/
object TimePoint {
  /**
    * List of default formats to try and parse.
    * 
    * It first tries day, month and then year from [[DateFormat]].
    */
  val defaultFormats = List(DateFormat.day, DateFormat.month, DateFormat.year)

  /** Create a TimePoint from DateTime.*/
  def apply(dt: DateTime): TimePoint = TimePoint(dt.getMillis)

  /** Parses a string as TimePoint using the specified pattern.*/
  def from(s: String, pattern: String): String \/ TimePoint =
    TimeParser.parse(s, pattern).map(TimePoint(_))

  /** Parses a string as TimePoint using the specified formatter.*/
  def from(s: String, formatter: DateTimeFormatter): String \/ TimePoint =
    TimeParser.parse(s, formatter).map(TimePoint(_))
  
  /** Parses a string as TimePoint using the default formatters.*/
  def fromDefault(s: String): String \/ TimePoint =
    TimeParser.parseDefault(s).map(TimePoint(_))
}
