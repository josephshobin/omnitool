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

import scalaz._, Scalaz._
import scalaz.\&/.{This, That}

import au.com.cba.omnia.omnitool.{Result, Ok, Error, OmnitoolTest}
import au.com.cba.omnia.omnitool.test.Arbitraries._
import au.com.cba.omnia.omnitool.test.OmnitoolProperties.resultantMonad

class ResultantMonadSpec extends OmnitoolTest with ResultantMatchers { def is = s2"""

Resultant Monad
===============

Resultant Monad should:
  obey resultant monad laws (monad and plus laws)                                            ${resultantMonad.laws[Resultant]}
  have safe mapping                                                                          $safeMap
  map                                                                                        $map
  have safe flatMap                                                                          $safeFlatMap
  map accross results                                                                        $rMap
  andThen                                                                                    $andThen
  plus and or are the same                                                                   $plus
  allow setting an error message                                                             $setMessage
  allow adding an error message                                                              $addMessage
  recoverWith for all cases is the same as or                                                $recoverWith
  recoverWith only recovers the specified error                                              $recoverWithSpecific
  onException does not change the result and performs the provided action on error           $onException
  ensure always perform the expected action                                                  $ensureAlwaysAction
  ensure fails if the action fails and returns either the original error or the action error $ensureError
  bracket is syntactic sugar for `ensure` and `flatMap`                                      $bracket

"""

  def safeMap = prop((x: Resultant[Int]) =>
    x.map[Int](_ => throw new Exception("Failure")) must beResultLike[Int] {
      case Error(_) => ok
      case _        => failure
    }
  )

  def map = prop((x: Resultant[Int]) =>
    x.map(_ + 1).map(_ - 1) must equal(x)
  )

  def safeFlatMap = prop((x: Resultant[Int]) =>
    x.flatMap[Int](_ => throw new Exception("Failure")) must beResultLike[Int] {
      case Error(_) => ok
      case _        => failure
    }
  )

  def rMap = prop((x: Resultant[Int]) =>
    x.rMap(r => r.map(_ + 1)).rMap(r => r.map(_ - 1)) must equal(x)
  )

  def andThen = prop((x: Resultant[Int]) =>
    x.andThen(i => Result.ok(i + 1)).map(_ - 1) must equal(x)
  )

  def plus = prop((x: Resultant[Int], y: Resultant[Int]) =>
    x <+> y must equal(x or y)
  )

  def setMessage = prop((x: Result[Int], msg: String) =>
    Resultant.result(x).setMessage(msg) must beResult(x.setMessage(msg))
  )
  
  def addMessage = prop((x: Result[Int], msg: String) =>
    Resultant.result(x).addMessage(msg) must beResult(x.addMessage(msg))
  )

  def recoverWith =  prop((x: Resultant[Int], y: Resultant[Int]) =>
    x.recoverWith { case _ => y} must equal (x or y)
  )

  def recoverWithSpecific = {
    val r = Result.fail[Int]("test")
    val a = Resultant(_ => r)
    val x = Resultant(_ => Result.ok(3))
    a.recoverWith { case This(_) =>  x } must beValue(3)
    a.recoverWith { case That(_) => x  } must beResult(r)
  }

  def onException = prop ((x: Resultant[Int]) => {
    var flag = false

    val actual = x.onException(Resultant(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    actual must beResultLike {
      case Ok(_)    => flag must beFalse
      case Error(_) => flag must beTrue
    }
  })

  def ensureAlwaysAction = prop ((x: Resultant[Int]) => {
    var flag = false
    val actual = x.ensure(Resultant(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    flag must beTrue

  })

  def ensureError = prop ((x: Resultant[Int], y: Resultant[Int]) => {
    x.ensure(y) must equal (x.flatMap(_ => y.flatMap(_ => x)))
  })

  def bracket = prop ((x: Resultant[Int], y: Resultant[Int], z: Resultant[Int]) =>
    x.bracket(_ => y)(_ => z) must equal (x.flatMap(_ => z).ensure(y))
  )
}
