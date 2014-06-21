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
  * A collection of methods to safely parse [[org.joda.time.DateTime]]s from Strings.
  */
object TimeParser {
  val defaultFormats = List(DateFormat.day, DateFormat.month, DateFormat.year)

  def parse(s: String, pattern: String): ValidationNel[String, DateTime] = {
    val formatter = DateTimeFormat.forPattern(pattern)
    Try(DateTime.parse(s, formatter).success)
      .getOrElse(s"Failed to parse $s as $pattern".failNel)
  }

  def parse(s: String, formatter: DateTimeFormatter): ValidationNel[String, DateTime] =
    Try(DateTime.parse(s, formatter).success)
      .getOrElse(s"Failed to parse $s using the given formatter".failNel)

  def parseDefault(s: String): ValidationNel[String, DateTime] = {
    def f(formatter: DateTimeFormatter) = parse(s, formatter).toOption
    defaultFormats
      .collectFirst(Function.unlift(f))
      .map(_.success)
      .getOrElse(s"Failed to parse $s using default formats".failNel)
  }
}
