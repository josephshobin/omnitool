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
  * A specialised monad `M[A]` for some operation that produces a `Result[A]`.
  *
  * `M` needs to be a monad relative to [[Result]] via `rPoint` and `rBind`.
  * `ResultantMonad` extends this relative monad into an ordinary monad.
  * See the _relative monad_ concept in "Monads need not be endofunctors" by Altenkirch, Chapman & Uustalu.
  * See the `DB` monad in CommBank/answer for a concrete example.
  */
trait ResultantMonad[M[_]] extends RelMonadPlus[Result, M] {

  implicit def R:  Monad[Result] = Result.ResultMonad
  implicit def PR: Plus[Result]  = Result.ResultPlus

  trait ResultantMonadLaw extends MonadLaw with PlusLaw

  def resultantMonadLaw: ResultantMonadLaw = new ResultantMonadLaw {}
}

object ResultantMonad {
  @inline def apply[M[_]](implicit M: ResultantMonad[M]): ResultantMonad[M] = M
}




