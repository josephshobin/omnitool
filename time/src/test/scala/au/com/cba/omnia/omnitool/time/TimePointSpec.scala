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

import au.com.cba.omnia.test.{OmniaSpec, TimeArbitrary}

class TimePointSpec extends OmniaSpec with TimeArbitrary { def is = s2"""
TimePoint
=========

A TimePoint can:
  be mapped over using a map on long $map
  be mapped over using a map on datetime $mapDateTime

"""
  def map = prop { (dt: Long, f: Long => Long) =>
    TimePoint(dt).map(f) must_== TimePoint(f(dt))
  }

  def mapDateTime = prop { (dt: DateTime, f: DateTime => DateTime) =>
    TimePoint(dt).mapDateTime(f) must_== TimePoint(f(dt))
  }
}
