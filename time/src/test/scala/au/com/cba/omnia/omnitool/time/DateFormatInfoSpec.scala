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

import Arbitraries._

case class FormatterWithFields(formatter: DateTimeFormatter, fields: Seq[DateTimeFieldType])

object DateFormatInfoSpec extends Spec { def is = s2"""
DateFormatInfoSpec
==============

DateFormatInfo should:
  Find all fields in random DateTimeFormatter    $findAllFieldsInRandom
  Find all fields in yyyyMMdd                    $findAllFieldsInLongYearMonthDay
  Find all fields in MMyy                        $findAllFieldsInShortYearMonth
"""

  def findAllFieldsInRandom = prop((formatterWithFields: FormatterWithFields) => {
    val FormatterWithFields(formatter, actualFields) = formatterWithFields
    DateFormatInfo.fields(formatter) must beLike {
      case Some(fields) => fields must contain(exactly(actualFields:_*))
    }
  })

  def findAllFieldsInLongYearMonthDay =
    DateFormatInfo.fields(DateFormat.yyyyMMdd) must beLike {
      case Some(fields) => fields must contain(exactly(
        DateTimeFieldType.year, DateTimeFieldType.monthOfYear, DateTimeFieldType.dayOfMonth
      ))
    }

  def findAllFieldsInShortYearMonth =
    DateFormatInfo.fields(DateTimeFormat.forPattern("MMyy")) must beLike {
      case Some(fields) => fields must contain(exactly(
        DateTimeFieldType.year, DateTimeFieldType.monthOfYear
      ))
    }
}
