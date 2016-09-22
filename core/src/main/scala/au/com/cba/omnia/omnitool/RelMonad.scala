//   Copyright 2015 Commonwealth Bank of Australia
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

import scalaz.{Monad, Id}
import scalaz.Scalaz.Id

/** Base trait for relative monads.
  *
  * Relative monad instances relate two functors in a way generalising the 
  * relationship between pure expressions and ordinary monads.
  *
  * Often we compose operations from one or more simple R's by choosing an
  * actual more complex M that has relative instances from each R.
  * 
  * Conceptual information about relative monads, including a formulation of
  * their laws, is described in the paper "Monads Need Not Be Endofunctors",
  * available at http://arxiv.org/pdf/1412.7148.pdf
  */
trait RelMonad[R[_], M[_]] {
  def rPoint[A](v: => R[A]): M[A]
  def rBind[A, B](ma: M[A])(f: R[A] => M[B]): M[B]
}

/** Methods and trivial instances for relative monads. */
object RelMonad {
  /** Every monad has a trivial self relative instance `RelMonad[R, R]`. */
  class SelfR[R[_]] extends RelMonad[R, R] {
    def rPoint[A](expr: => R[A]): R[A]               = expr
    def rBind[A, B](ra: R[A])(f: R[A] => R[B]): R[B] = f(ra)
  }

  /** Ordinary monads are relative to [[scalaz.Id]]. Note `Id[A] = A` directly. */
  class IdR[R[_]](implicit R: Monad[R]) extends RelMonad[Id, R] {
    def rPoint[A](expr: => Id[A]): R[A]                = R.point(expr)
    def rBind[A, B](ra: R[A]) (f: Id[A] => R[B]): R[B] = R.bind(ra)(f)
  }

  /** Relatives to [[scalaz.Id]] are ordinary monads.  Note `Id[A] = A`. */
  class MonadIdR[R[_]](R: RelMonad[Id, R]) extends Monad[R] {
    def point[A](expr: => A): R[A]                    = R.rPoint(expr)
    def bind[A, B](ra: R[A]) (f: Id[A] => R[B]): R[B] = R.rBind(ra)(f)
  }

  /** Composition of RelMonad instances (with N & R monads) */
  class ComposeR[M[_], N[_], R[_]](
    MrelN: RelMonad[M, N], NrelR: RelMonad[N, R]
  )(implicit N: Monad[N], R: Monad[R]) extends RelMonad[M, R] {
    def rPoint[A](expr: => M[A]): R[A] =
      NrelR.rPoint(MrelN.rPoint(expr))

    def rBind[A, B](ra: R[A])(f: M[A] => R[B]): R[B] =
      NrelR.rBind(ra)((na: N[A]) => R.join(NrelR.rPoint(
        MrelN.rBind(na)(ma => N.point(f(ma)))
      )))
  }

  /** Relative map, handy for lifting functions from M to N */
  def rMap[A, B, M[_], N[_]](na: N[A])(f: M[A] => M[B])(
    MrelN: RelMonad[M, N]
  ): N[B] = {
    MrelN.rBind(na)(ma => MrelN.rPoint(f(ma)))
  }
}
