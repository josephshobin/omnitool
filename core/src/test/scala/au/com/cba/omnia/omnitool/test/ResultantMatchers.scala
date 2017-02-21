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

import scala.util.Random

import scalaz.Equal

import org.specs2.Specification
import org.specs2.matcher.Matcher
import org.specs2.execute.{Result => SpecResult}

import au.com.cba.omnia.omnitool.Result

/** Convenient matchers for [[Resultant]]. */
trait ResultantMatchers { self: Specification =>
  def beResult[A](expected: Result[A]): Matcher[Resultant[A]] =
    (h: Resultant[A]) => h.f(Random.nextInt) must_== expected

  def beResultLike[A](expected: Result[A] => SpecResult): Matcher[Resultant[A]] =
    (h: Resultant[A]) => expected(h.f(Random.nextInt))

  def beValue[A](expected: A): Matcher[Resultant[A]] =
    beResult(Result.ok(expected))

  def equal[A : Equal](expected: A): Matcher[A] =
    (h: A) => implicitly[Equal[A]].equal(h, expected)

}
