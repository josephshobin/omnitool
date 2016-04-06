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

import scalaz.{Monad, ReaderT, ~>}
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
  *
  * R and M should generally be functors, but as for scalaz ~> this isn't required.
  */
trait RelMonad[R[_], M[_]] extends (R ~> M) {
  def rPoint[A](v: => R[A]): M[A]
  def rBind[A, B](ma: M[A])(f: R[A] => M[B]): M[B]

  /** Relative map, handy for lifting functions from M to N */
  def rMap[A, B](ma: M[A])(f: R[A] => R[B]): M[B] = {
    rBind(ma)(ra => rPoint(f(ra)))
  }

  def apply[A](ra: R[A]): M[A]       = rPoint[A](ra)      // Strict in ra as per ~>
  def apply[A](ra: () => R[A]): M[A] = rPoint[A](ra ())   // For call by name
}

/** Import %~>._ to write R %~> M instead of RelMonad[R, M] */
object %~> {
  type %~>[R[_], M[_]] = RelMonad[R, M]   // Alias the trait
  val  %~>             = RelMonad         // Alias the companion object
}

/** Methods and trivial instances for relative monads. */
object RelMonad {

  /** Every monad has a trivial self relative instance `RelMonad[R, R]`. */
  class SelfR[R[_]] extends RelMonad[R, R] {
    def rPoint[A](expr: => R[A]): R[A]               = expr
    def rBind[A, B](ra: R[A])(f: R[A] => R[B]): R[B] = f(ra)
  }
  object SelfR { def apply[R[_]]: RelMonad[R, R] = new SelfR[R] }

  /** Ordinary monads are relative to `Id`. Note `Id[A] = A` directly. */
  class IdR[R[_]](implicit R: Monad[R]) extends RelMonad[Id, R] {
    def rPoint[A](expr: => Id[A]): R[A]                = R.point(expr)
    def rBind[A, B](ra: R[A]) (f: Id[A] => R[B]): R[B] = R.bind(ra)(f)
  }
  object IdR { def apply[R[_] : Monad]: RelMonad[Id, R] = new IdR[R] }

  /** Relatives to `Id` are ordinary monads, since `Id[A] = A`. */
  class MonadIdR[R[_]](implicit R: RelMonad[Id, R]) extends Monad[R] {
    def point[A](expr: => A): R[A]                    = R.rPoint(expr)
    def bind[A, B](ra: R[A]) (f: A => R[B]): R[B] = R.rBind(ra)(f)
  }
  object MonadIdR { def apply[R[_]](implicit R: RelMonad[Id, R]) = new MonadIdR[R] }

  /** Composition of RelMonad instances (with N & R monads). */
  def ComposeR[M[_], N[_], R[_]](
    MrelN: RelMonad[M, N], NrelR: RelMonad[N, R]   // Could be ambiguous if implicit.
  )(implicit N: Monad[N], R: Monad[R]) = new RelMonad[M, R] {
    def rPoint[A](expr: => M[A]): R[A] =
      NrelR.rPoint(MrelN.rPoint(expr))

    def rBind[A, B](ra: R[A])(f: M[A] => R[B]): R[B] =
      NrelR.rBind(ra)((na: N[A]) => R.join(NrelR.rPoint(
        MrelN.rBind(na)(ma => N.point(f(ma)))
    )))
  }

  /** Relative map, handy for lifting functions from M to N */
  def rMap[A, B, M[_], N[_]](na: N[A])(f: M[A] => M[B])(implicit MrelN: RelMonad[M, N]): N[B] =
    MrelN.rBind(na)(ma => MrelN.rPoint(f(ma)))

  /** extend RelMonad[M, N] instances to RelMonad[M, ReaderT[N, Rd, _]] */
  // It may be simpler to derive this by composition from RelMonad[N, ReaderT[N, Rd, _]]
  class ReaderTR[M[_] : Monad, N[_] : Monad, Rd](MrelN: RelMonad[M, N])
  extends RelMonad[M, ({ type readerN[A]=ReaderT[N, Rd, A] })#readerN] {
    val N = Monad[N]

    type ReaderN[A] = ReaderT[N, Rd, A]
    val readerN = Monad[ReaderN]
    def mkReaderN[A](f: Rd => N[A]) = new ReaderT[N, Rd, A](f)

    def rPoint[A](v: => M[A]): ReaderN[A] = new ReaderT(_ => MrelN.rPoint(v))
    def rBind[A, B](rNA: ReaderN[A])(f: M[A] => ReaderN[B]): ReaderN[B] =
      readerN.join(mkReaderN[ReaderN[B]](rd =>
                     MrelN.rBind(rNA(rd))(ma => N.point(f(ma)))
                   ))
    val _: RelMonad[M, ReaderN] = this  // Check the type in a nicer form.
  }

  /** A trait for "self composing" RelMonad instances. */
  trait GenRelMonad[R[_], M[_]] {
    def compose[L[_]](implicit LRelR: RelMonad[L, R]): RelMonad[L, M]
    def self: RelMonad[R, M] = compose[R](SelfR[R])
  }
}
