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
import org.scalacheck._, Gen._, Arbitrary._

import scalaz._, Scalaz._

object Arbitraries {
  implicit lazy val ArbitraryTimePoint: Arbitrary[TimePoint] =
    Arbitrary(arbitrary[Long] map (l => TimePoint(math.abs(l))))

  /** Restricted formatter to fields y, M, d, H, m, and s */
  implicit lazy val ArbitraryFormatterWithFields: Arbitrary[FormatterWithFields] =
    Arbitrary(
      someOf(List(
        DateTimeFieldType.year.right,
        DateTimeFieldType.monthOfYear.right,
        DateTimeFieldType.dayOfMonth.right,
        DateTimeFieldType.hourOfDay.right,
        DateTimeFieldType.minuteOfHour.right,
        DateTimeFieldType.secondOfMinute.right,
        "-".left,
        ":".left,
        " ".left,
        "foo".left,
        "bar".left
      ))
        .map    { parts                  => (builderFromParts(parts), parts.flatMap(_.toList)) }
        .filter { case (builder, _)      => builder.canBuildFormatter && builder.canBuildParser && builder.canBuildPrinter }
        .map    { case (builder, fields) => FormatterWithFields(builder.toFormatter, fields) }
    )

  def builderFromParts(parts: Seq[String \/ DateTimeFieldType]): DateTimeFormatterBuilder = {
    def addPart(builder: DateTimeFormatterBuilder, x: String \/ DateTimeFieldType) =
      x.fold(
        l =>                                                  builder.appendLiteral(l),
        t =>
          if      (t equals DateTimeFieldType.year)           builder.appendYear(4, 4)
          else if (t equals DateTimeFieldType.monthOfYear)    builder.appendMonthOfYear(2)
          else if (t equals DateTimeFieldType.dayOfMonth)     builder.appendDayOfMonth(2)
          else if (t equals DateTimeFieldType.hourOfDay)      builder.appendHourOfDay(2)
          else if (t equals DateTimeFieldType.minuteOfHour)   builder.appendMinuteOfHour(2)
          else if (t equals DateTimeFieldType.secondOfMinute) builder.appendSecondOfMinute(2)
          else                                                throw new Exception(s"Unexpected field type: $t")
      )

    parts.foldLeft(new DateTimeFormatterBuilder())(addPart(_, _))
  }
}
