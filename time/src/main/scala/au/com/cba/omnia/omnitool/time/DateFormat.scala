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

import org.joda.time._
import org.joda.time.format._

/** A collection of joda-time DateTimeFormatters. */
object DateFormat {
  /** DateTimeFormatter for `yyyy-MM-dd`, e.g. `2012-03-27`. */
  lazy val yyyyMMdd: DateTimeFormatter =
    DateTimeFormat.forPattern("yyyy-MM-dd")

  /** DateTimeFormatter for `yyyy-MM`, e.g. `2012-03`. */
  lazy val yyyyMM: DateTimeFormatter =
    DateTimeFormat.forPattern("yyyy-MM")

  /** DateTimeFormatter for `yyyy`, e.g. `2012`. */
  lazy val yyyy: DateTimeFormatter =
    DateTimeFormat.forPattern("yyyy")

  /** DateTimeFormatter for `yyyy-MM-dd`, e.g. `2012-03-27`. */
  lazy val day: DateTimeFormatter = yyyyMMdd

  /** DateTimeFormatter for `yyyy-MM`, e.g. `2012-03`. */
  lazy val month: DateTimeFormatter = yyyyMM

  /** DateTimeFormatter for `yyyy`, e.g. `2012`. */
  lazy val year: DateTimeFormatter = yyyy

  /** DateTimeFormatter for week of year, e.g. `2012-week-43`. */
  lazy val week: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendYear(4,4)
      .appendLiteral("-week-")
      .appendWeekOfWeekyear(2)
      .toFormatter
}
