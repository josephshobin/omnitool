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

import scalaz.{Monad, Plus, MonadError}
import scalaz.\&/._

/**
  * A specialised monad `M[A]` for some operation that produces a `Result[A]`.
  *
  * `M` needs to be a monad relative to [[Result]] via `rPoint` and `rBind`.
  * `ResultantMonad` extends this relative monad into an ordinary monad.
  * See the _relative monad_ concept in "Monads need not be endofunctors" by Altenkirch, Chapman & Uustalu.
  * See the `DB` monad in CommBank/answer for a concrete example.
  */
object ResultantMonad {
 implicit class ResultantMonad[M[_]](RM: RelMonad[Result, M]) extends Plus[M] with Monad[M]
    with MonadError[({type F[_, A] = M[A]})#F, These[String, Throwable]] {
  /** Similar to a `Monad.point` but expects a `Result`. */
  def rPoint[A](v: => Result[A]): M[A] = RM.rPoint(v)

  /** Similar to a `Monad.bind` but binds a `Result`. */
  def rBind[A, B](ma: M[A])(f: Result[A] => M[B]): M[B] = RM.rBind(ma)(f)

  /** `Monad.point` also called `return`. */
  def point[A](v: => A): M[A] = rPoint(Result.safe(v))

  /** `Monad.bind` also called `flatMap`. */
  def bind[A, B](ma: M[A])(f: A => M[B]): M[B] = 
    rBind(ma)(x => try {
      x.map(f).fold(
        identity,
        errors => rPoint(Result.these[B](errors))
      )
    } catch {
      case NonFatal(ex) => rPoint(Result.error("Exception occurred inside a bind", ex))
    })

  /**
    * `MonadPlus.plus`. Evaluates `alternative` if `ma` fails.
    *
    * Returns the error of `alternative` iff both `ma` and `alternative` fail.
    */
  def plus[A](ma: M[A], alternative: => M[A]): M[A] = 
    rBind(ma)(a => a.fold(
      _ => rPoint(a),
      _ => alternative
    ))

  type errTy = These[String, Throwable]

  // Overrides for MonadError
  override def handleError[A](ma: M[A])(recovery: errTy ⇒ M[A]): M[A] =  // TODO: refactor
    rBind(ma)(r => r.fold(
               _     => rPoint(r),
               error => recovery(error)
    ))
  override def raiseError[A](errors: errTy) = rPoint(Result.these[A](errors))

  trait ResultantMonadLaw extends MonadLaw with PlusLaw

  def resultantMonadLaw: ResultantMonadLaw = new ResultantMonadLaw {}
}
}
// object ResultantMonad {
//   @inline def apply[M[_]](implicit M: ResultantMonad[M]): ResultantMonad[M] = M

//   implicit def fromRelMonad[RR[_]](RR: RelMonad[Result, RR]): ResultantMonad[RR] = new ResultantMonad[RR] {
//     def rPoint[A](v: => Result[A]): RR[A] = RR.rPoint(v)
//     def rBind[A, B](rA: RR[A])(f: Result[A] => RR[B]): RR[B] = RR.rBind(rA)(f)
//   }

//}
