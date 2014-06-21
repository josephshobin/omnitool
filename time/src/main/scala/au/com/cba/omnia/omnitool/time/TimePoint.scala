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

import scala.util.Try

import org.joda.time._
import org.joda.time.format._

import scalaz._, Scalaz._


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
  def toDateTime: DateTime = new DateTime(instant, DateTimeZone.UTC)

  def toString(format: DateTimeFormatter): String = toDateTime.toString(format)

  def day: String = toString(DateFormat.day)
  def month: String = toString(DateFormat.month)
  def year: String = toString(DateFormat.year)

  def map(f: Long => Long) = TimePoint(f(instant))
  def mapDateTime(f: DateTime => DateTime) = TimePoint(f(toDateTime))
}

object TimePoint {
  val defaultFormats = List(DateFormat.day, DateFormat.month, DateFormat.year)

  def apply(dt: DateTime): TimePoint = TimePoint(dt.getMillis)

  def from(s: String, pattern: String): ValidationNel[String, TimePoint] =
    TimeParser.parse(s, pattern).map(TimePoint(_))

  def from(s: String, formatter: DateTimeFormatter): ValidationNel[String, TimePoint] =
    TimeParser.parse(s, formatter).map(TimePoint(_))
  
  def fromDefault(s: String): ValidationNel[String, TimePoint] =
    TimeParser.parseDefault(s).map(TimePoint(_))
}
