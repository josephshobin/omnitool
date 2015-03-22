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

import scala.util.control.NonFatal

import scalaz.{Monad, Plus}

/**
  * An extension of RelMonad to also derive `plus` when the underlying monad
  * includes `plus`.
  */
trait RelMonadPlus[R[_], M[_]] extends Plus[M] with RelMonad[R, M] {

  implicit def PR: Plus[R]
  implicit def R:  Monad[R]

  /**
    * `RelMonadPlus.plus`. Evaluates `alternative` if `ma` fails.
    *
    * Returns the error of `ma` iff both `ma` and `alternative` fail.
    */
  def plus[A](ma: M[A], alternative: => M[A]): M[A] =
    rBind(ma)((ra: R[A]) => PR.plus(R.map(ra)((a:A) => rPoint(R.point(a))), R.point(alternative)))

  trait RelMonadPlusLaw extends MonadLaw with PlusLaw

  def relMonadPlusLaw: RelMonadPlusLaw = new RelMonadPlusLaw {}
}

object RelMonadPlus {
  @inline def apply[R[_], M[_]](implicit M: RelMonadPlus[R, M]): RelMonadPlus[R, M] = M
}
