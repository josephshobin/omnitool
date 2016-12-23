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

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scalaz._, Scalaz._

import com.cba.omnia.test.{OmniaSpec, TimeArbitrary}

class TimeParserSpec extends OmniaSpec with TimeArbitrary { def is = s2"""
TimeParser
=========

A TimeParser can:
  parse given a DateTimeFormat instance $parse
  parse given a pattern string          $parseString
  parse using default patterns          $parseDefault

"""
  def parse = prop { (dt: DateTime) =>
    TimePoint.defaultFormats.foreach { pattern =>
      val s = dt.toString(pattern)
      val fs = s ++ "aa"
      TimeParser.parse(s, pattern)  must_== DateTime.parse(s, pattern).right
      TimeParser.parse(fs, pattern) must_== s"Failed to parse $fs using the given formatter".left
    }

    ok
  }

  def parseString = prop { (dt: DateTime) =>
    List("yyyy-MM-dd", "yyyy-MM").foreach { pattern =>
      val s = dt.toString(pattern)
      val fs = s ++ "aa"
      TimeParser.parse(s, pattern)  must_== DateTime.parse(s, DateTimeFormat.forPattern(pattern)).right
      TimeParser.parse(fs, pattern) must_== s"Failed to parse $fs as $pattern".left
    }

    val s = "2013-06-09"
    val pattern = "aaa"
    TimeParser.parse(s, pattern) must_== s"Failed to parse $s as $pattern".left
  }

  def parseDefault = prop { (dt: DateTime) =>
    TimeParser.defaultFormats.foreach { pattern =>
      val s = dt.toString(pattern)
      val fs = s ++ "aa"
      TimeParser.parseDefault(s)  must_== DateTime.parse(s, pattern).right
      TimeParser.parseDefault(fs) must_== s"Failed to parse $fs using default formats".left
    }

    ok
  }
}
