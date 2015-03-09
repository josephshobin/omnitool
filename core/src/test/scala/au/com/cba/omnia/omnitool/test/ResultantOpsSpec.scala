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

package au.com.cba.omnia.omnitool.test

import scalaz.\&/.{Both, This, That}
import scalaz.scalacheck.ScalazProperties.{monad, plus}

import org.specs2.{Specification, ScalaCheck}
import org.specs2.matcher.{TerminationMatchers, ThrownExpectations}

import au.com.cba.omnia.omnitool.{Result, Ok, Error, OmnitoolTest}
import au.com.cba.omnia.omnitool.ResultantMonadSyntax._
import au.com.cba.omnia.omnitool.test.Arbitraries._

class ResultantOpsSpec extends OmnitoolTest with ResultantMatchers { def is = s2"""

Resultant Ops
=============

Resultant Ops should:
  lift a value $value
  lift a value safely $safeValue
  lift a result $result
  lift an error (error message only) $fail
  lift an exception $exception
  lift an error (message and exception) $error
  fail with a specified error message if a condition is not met $guard
  prevent is the opposite of guard $prevent
  mandatory is the same as guard for lifted conditions $mandatory
  forbidden is the opposite of mandatory $forbidden

"""

  def value = prop ((x: Int) =>
    Resultant.value[Int](x) must beValue(x)
  )

  def safeValue = prop ((s: String) => {
    val x = new Exception(s)
    Resultant.value(throw x) must beResult(Result.safe(throw x))
  })

  def result = prop ((x: Result[Int]) =>
    Resultant.result(x) must beResult(x)
  )

  def fail = prop ((s: String) =>
    Resultant.fail[Int](s) must beResultLike {
      case Error(This(msg)) => msg must_== s
      case _                => failure
    }
  )

  def exception = prop ((s: String) => {
    val x = new Exception(s)
    Resultant.exception[Int](x) must beResultLike {
      case Error(That(ex)) => ex must_== x
      case _               => failure

    }
  })

  def error =  prop ((a: String, b: String) => {
    val x = new Exception(b)
    Resultant.error[Int](a, x) must beResultLike {
      case Error(Both(msg, ex)) => (msg, ex) must_== ((a, x))
      case _                    => failure

    }
  })

  def guard = prop ((b: Boolean, msg: String) =>
    Resultant.guard(b, msg) must equal(
      if (b) Resultant.value[Unit](())
      else   Resultant.fail[Unit](msg)
    )
  )

  def prevent = prop ((b: Boolean, msg: String) =>
    Resultant.prevent(b, msg) must equal(Resultant.guard(!b, msg))
  )

  def mandatory = prop ((b: Boolean, msg: String) =>
    Resultant.mandatory(Resultant.value(b), msg) must equal(Resultant.guard(b, msg))
  )

  def forbidden = prop ((b: Resultant[Boolean], msg: String) =>
    Resultant.forbidden(b, msg) must equal(Resultant.mandatory(b.map(!_), msg))
  )
}

