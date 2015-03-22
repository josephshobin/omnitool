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

// import scala.util.control.NonFatal

import scalaz.Monad

/**
  * A specialised monad `M[A]` defined relative to another monad `R[A]`.
  *
  * `M` needs to be a monad relative to [[R]] via `rPoint` and `rBind`.
  * `RelaKsMonad` extends this relative monad into an ordinary monad.
  * See the _relative monad_ concept in "Monads need not be endofunctors" by Altenkirch, Chapman & Uustalu.
  * See the `DB` monad in CommBank/answer for a concrete example.
  */
trait RelaKsMonad[R[_], M[_]] extends Monad[M] {

  implicit def R: Monad[R]

  /** Similar to a `Monad.point` but expects an `R[A]`. */
  def rPoint[A](v: => R[A]): M[A]

  /** Similar to a `Monad.bind` but binds an `R[A]`. */
  //def rBind[A, B](ma: M[A])(f: R[A] => M[B]): M[B]

  /* The above aren't enough for `M` to be a monad.
   * There's some choice on how to strengthen them so it is.  */

  /** Alt 1. Similar to a `Monad.bind` but binds an `R[A]`. */
  def rBind[A, B](ma: M[A])(f: R[A] => R[M[B]]): M[B]

  /** Alt 2. Essential here, but trivial in the standard case of R=Identity. */
  //def rJoin[A](rma: R[M[A]]): M[A]

  /** Alt 3. Perhaps the least extra required? **/
  // def join[A](mma: M[M[A]]): M[A]

  /** `Monad.point` also called `return`. */
  def point[A](v: => A): M[A] = rPoint(R.point(v))

  // /** `Monad.bind` also called `flatMap`. */
  // def bind[A, B](ma: M[A])(f: A => M[B]): M[B] = {
  //   rBind(ma)(ra => rJoin(
  //     R.bind(ra)(a => R.point(f(a)))
  //   ))
  // }

  /** `Monad.bind` also called `flatMap`. */
  // def bindViaJoin[A, B](ma: M[A])(f: A => M[B]): M[B] = {
  //   rBind(ma)(ra => join(rPoint(
  //     R.bind(ra)(a => R.point(f(a)))
  //   )))
  // }

  /** `Monad.bind` based on rrBind. */
  def bind[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    rBind(ma)(ra =>
      R.bind(ra)(a => R.point(f(a)))
    )
  }



//    val mmb: M[M[B]] = rBind(ma)((ra: R[A]) => rPoint(R.bind(ra)((a: A) => R.point(f(a)))))
//    rBind(mmb)((rmb: R[M[B]]) => R.bind(rmb)((mb: M[B]) => mb))

      //R.bind(ra)(f))


  // R.point: (=> A) => R[A]
  // R.bind: R[A] => (A => R[B]) => R[B]

  /** `Monad.bind` also called `flatMap`. */
  // def bind[A, B](ma: M[A])(f: A => M[B]): M[B] =
  //   rBind(ma)(x =>
  //     x.map(f).fold(
  //       identity,
  //       errors => rPoint(Result.these[B](errors))
  //     ))




//  trait RelaKsMonadLaw extends MonadLaw

//  def relaKsMonadLaw: RelaKsMonadLaw = new RelaKsMonadLaw {}
}

//object RelaKsMonad {
//  @inline def apply[M[_]](implicit M: RelaKsMonad[R, M]): RelaKsMonad[R, M] = M
//}
