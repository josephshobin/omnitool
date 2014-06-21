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

import com.cba.omnia.test.TimeArbitrary._
import com.cba.omnia.test.Spec

import org.joda.time._
import org.joda.time.format._
import org.scalacheck._, Gen._
import org.specs2.mutable.Tables

object DateFormatSpec extends Spec with Tables { def is = s2"""
DateFormatSpec
==============

All formats should:
  Parse known answers                             $known
  Fail with joda errors for invalid formats       $fails

The `yyyy-MM-dd` format should:
  Be symmetric in print/parse                     $symmetric

"""

  def known =
    "Date String"   | "Expected"                  | "Formatter"            |>
    "2012-01-01"    ! new LocalDate(2012,  1,  1) !  DateFormat.yyyyMMdd   |
    "2013-12-31"    ! new LocalDate(2013, 12, 31) !  DateFormat.yyyyMMdd   | {
      (date: String, expected: LocalDate, formatter: DateTimeFormatter) =>
        formatter.parseLocalDate(date) must_== expected
    }

  def fails =
    "Date String"   | "Formatter"            |>
    " 2012-01-01"   !  DateFormat.yyyyMMdd   |
    "2013-12-31 "   !  DateFormat.yyyyMMdd   | {
      (date: String, formatter: DateTimeFormatter) =>
        formatter.parseLocalDate(date) must throwA[IllegalArgumentException]
    }

  def symmetric = prop((date: LocalDate) =>
    DateFormat.yyyyMMdd.parseLocalDate(DateFormat.yyyyMMdd.print(date)) must_== date)
}
