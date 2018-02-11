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

import scalaz._, Scalaz._

/** A collection of methods to safely parse DateTimes from Strings.*/
object TimeParser {
  /**
    * List of default formats to try and parse.
    * 
    * It first tries day, month and then year from [[DateFormat]].
    */
  val defaultFormats = List(DateFormat.day, DateFormat.month, DateFormat.year)

  /** Parses a string as DateTime using the specified pattern.*/
  def parse(s: String, pattern: String): String \/ DateTime =
    \/.fromTryCatchThrowable[DateTime, Exception] {
      val formatter = DateTimeFormat.forPattern(pattern)
      DateTime.parse(s, formatter)
    }.leftMap(_ => s"Failed to parse $s as $pattern")

  /** Parses a string as DateTime using the specified formatter.*/
  def parse(s: String, formatter: DateTimeFormatter): String \/ DateTime =
    \/.fromTryCatchThrowable[DateTime, Exception] {
      DateTime.parse(s, formatter)
    }.leftMap(_ => s"Failed to parse $s using the given formatter")

  /** Parses a string as DateTime using the default formatters.*/
  def parseDefault(s: String): String \/ DateTime = {
    def f(formatter: DateTimeFormatter) = parse(s, formatter).toOption
    defaultFormats
      .collectFirst(Function.unlift(f))
      .map(_.right)
      .getOrElse(s"Failed to parse $s using default formats".left)
  }
}
