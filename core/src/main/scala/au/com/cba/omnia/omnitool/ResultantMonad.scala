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
  // {
  //   def rPoint[A](v: => Result[A]): M[A]                  = RM.rPoint(v)
  //   def rBind[A, B](ma: M[A])(f: Result[A] => M[B]): M[B] = RM.rBind(ma)(f)
  // }
}

  //}
  // object ResultantMonad {
  //   trait ToMonadOps extends ToMonadOps0 with ToApplicativeOps with ToBindOps {
  //     implicit def ToMonadOps[F[_],A](v: F[A])(implicit F0: Monad[F]) =
  //       new MonadOps[F,A](v)

  //     ////

  //     ////
  //   }

  //   implicit def ToScalazMonadOps[M[_], A](v: M[A])(implicit ResultRel: RelMonad[Result, M]): MonadSyntax[M]
  //     new 






//   implicit def fromRelMonad[RR[_]](RR: RelMonad[Result, RR]): ResultantMonad[RR] = new ResultantMonad[RR] {
//     def rPoint[A](v: => Result[A]): RR[A] = RR.rPoint(v)
//     def rBind[A, B](rA: RR[A])(f: Result[A] => RR[B]): RR[B] = RR.rBind(rA)(f)
//   }

//}
