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

import com.github.nscala_time.time.OrderingImplicits._

import org.joda.time._

/**
  * A collection of `Ordering` instances for use
  * in explicit sort orders methods when the default nscala
  * instances don't cut it (or don't exist).
  */
object DateOrder {
  /** Chronological ordering for [[org.joda.time.LocalDate]].*/
  lazy val LocalDateChronological: Ordering[LocalDate] =
    implicitly[Ordering[LocalDate]]

  /** Reverse Chronological ordering for [[org.joda.time.LocalDate]].*/
  lazy val LocalDateReverseChronological: Ordering[LocalDate] =
    LocalDateChronological.reverse

  /** Chronological ordering for [[TimePoint]].*/
  lazy val TimePointChronological: Ordering[TimePoint] =
    Ordering.fromLessThan(_.instant < _.instant)

  /** Reverse chronological ordering for [[TimePoint]].*/
  lazy val TimePointReverseChronological: Ordering[TimePoint] =
    TimePointChronological.reverse
}
