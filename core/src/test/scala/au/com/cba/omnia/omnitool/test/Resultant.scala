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

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import au.com.cba.omnia.omnitool.{Result, ResultantMonad, ResultantMonadOps, ResultantOps, ToResultantMonadOps}
import au.com.cba.omnia.omnitool.test.Arbitraries._

/** Dumpy implementation of an instance of a ResultantMonad. */
case class Resultant[A](f: Int => Result[A]) {
  override def toString = s"Resultant(_ => ${f(1).toString})"
}

/** Arbitraries and typeclass implementations of [[Resultant]]. */
object Resultant extends ResultantOps[Resultant] with ToResultantMonadOps {
  implicit val monad: ResultantMonad[Resultant] = new ResultantMonad[Resultant] {
    /** Similar to a `Monad.point` but expects a `Result`. */
    def rPoint[A](v: => Result[A]): Resultant[A] = Resultant(_ => v)

    /** Similar to a `Monad.bind` but expects a `Result`. */
    def rBind[A, B](ma: Resultant[A])(f: Result[A] => Resultant[B]): Resultant[B] =
      Resultant(x => f(ma.f(x)).f(x))
  }

  implicit def ResultantEqual[A]: Equal[Resultant[A]] = {
    val i = Random.nextInt
    Equal.equal[Resultant[A]]((a, b) => a.f(i) == b.f(i))
  }

  implicit def ResultantArbitrary[A : Arbitrary]: Arbitrary[Resultant[A]] =
    Arbitrary(arbitrary[Result[A]].map(r => Resultant(_ => r)))
}
