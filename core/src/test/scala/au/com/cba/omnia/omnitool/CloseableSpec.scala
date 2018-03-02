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

package au.com.cba.omnia.omnitool

import Closeable._

class CloseableSpec extends OmnitoolTest { def is = s2"""

Error handling
==============

Closeable:
  Can be used to perform an operation on a Closeable and then close it $doAndRelease
  Can be used to perform an operation on a resource and then close it if a close operation has been supplied $doAndReleaseManual

"""

  class TestClose(var count: Int, var closed: Boolean = false) {
    def inc = {
      count += 1
      count
    }

    def close() = if (!closed) closed = true else throw new Exception("called close twiece")

  }

  object TestClose {
    implicit def TestCloseToCloseable = Closeable[TestClose]((x: TestClose) => x.close)
  }

  def doAndRelease = {
    val x = new TestClose(0)
    val count = x.doAndRelease(_.inc)
    count === 1
    x.count === 1
    x.closed must beTrue
  }

  def doAndReleaseManual = {
    val x = new TestClose(0)
    val count = Closeable.doAndRelease[TestClose, Int](x, _.inc, _.close)
    count === 1
    x.count === 1
    x.closed must beTrue
  }
}
