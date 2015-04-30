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

import scalaz._
import scalaz.scalacheck.ScalazArbitrary._

class ValidatedSpec extends OmnitoolTest { def is = s2"""

Error handling
==============

Validated:
  can be used to wrap operations that might throw exceptions            $safe
  allow for adding additional error messages to already existing errors $addError

"""

  def safe = {
    Validated.safe("a".toInt) must_== Failure(NonEmptyList("java.lang.NumberFormatException: For input string: \"a\""))
    Validated.safe("3".toInt) must_== Success(3)
  }

  def addError = prop { (msg: String, a: Validated[Int]) =>
    a.addError(msg) must beLike {
      case Success(x) => a match {
        case Success(`x`) => ok
        case _           => ko
      }
      case Failure(x) => a match {
        case Failure(list) => x.list === (msg :: list.list)
        case _        => ko
      }
    }
  }
}
