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

import com.github.nscala_time.time.Imports._

import org.joda.time.LocalDate

import org.specs2.mutable.Tables
import org.specs2.execute.Result

import com.cba.omnia.test.TimeArbitrary._
import com.cba.omnia.test.Spec

import DateOrder._
import Arbitraries._

object DateOrderSpec extends Spec with Tables { def is = s2"""
DateOrderSpec
==============

TimePoint should:
  sort in chronological order                     $timePointChronological
  sort in reverse chronological order             $timePointReverseChronological

LocalDate should:
  sort in chronological order                     $localDateChronological
  sort in reverse chronological order             $localDateReverseChronological

"""

  def timePointChronological = prop((l: List[TimePoint]) => !l.isEmpty ==> {
    contextually(l.sorted(TimePointChronological))((last, current) =>
      current.instant must be_>= (last.instant))
  })

  def timePointReverseChronological = prop((l: List[TimePoint]) => !l.isEmpty ==> {
    contextually(l.sorted(TimePointReverseChronological))((last, current) =>
      current.instant must be_<= (last.instant))
  })

  def localDateChronological = prop((l: List[LocalDate]) => !l.isEmpty ==> {
    contextually(l.sorted(LocalDateChronological))((last, current) =>
      (current == last || current.isAfter(last)) must beTrue)
  })

  def localDateReverseChronological = prop((l: List[LocalDate]) => !l.isEmpty ==> {
    contextually(l.sorted(LocalDateReverseChronological))((last, current) =>
      (current == last || current.isBefore(last)) must beTrue)
  })

  def contextually[A, B](l: List[A])(check: (A, A) => B): Result = {
    l.reduceLeft((last, v) => { check(last, v); v })
    ok
  }
}
