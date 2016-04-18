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


// The earlier ResultantMonad has been replaced by a type definition via RelMonad[Result, _] in the
// package object, and enrichment via RichResultantMonad (below) in ToResultantMonadOps.

package au.com.cba.omnia.omnitool

import scala.util.control.NonFatal

import scalaz.{MonadPlus, Monad, Plus, MonadError}
import scalaz.\&/._

import scalaz.\&/.These

trait ResultantMonadPackage {
  type ResultantMonad[M[_]] = RelMonad[Result, M]
}


// Enrich RelMonad[Result, M] with ResultantMonad specific methods and traits.
class RichResultantMonad[M[_]](RM: RelMonad[Result, M])
extends MonadAndPlus[M] with MonadError[({type F[_, A] = M[A]})#F, These[String, Throwable]] {

  /** `Monad.point` also called `return`. */
  def point[A](v: => A): M[A] = RM.rPoint(Result.safe(v))

  /** `Monad.bind` also called `flatMap`. */
  def bind[A, B](ma: M[A])(f: A => M[B]): M[B] = 
    RM.rBind(ma)(x => try {
      x.map(f).fold(
        identity,
        errors => RM.rPoint(Result.these[B](errors))
      )
    } catch {
      case NonFatal(ex) => RM.rPoint(Result.error("Exception occurred inside a bind", ex))
    })

  /**
    * `MonadPlus.plus`. Evaluates `alternative` if `ma` fails.
    *
    * Returns the error of `alternative` iff both `ma` and `alternative` fail.
    */
  def plus[A](ma: M[A], alternative: => M[A]): M[A] = 
    RM.rBind(ma)(a => a.fold(
      _ => RM.rPoint(a),
      _ => alternative
    ))
  //def empty[A] = RM.RM.rPoint(Result.fail("Failure triggered by use of RichResultantMonad.empty in omnitool"))

  // Overrides for MonadError
  type errTy = These[String, Throwable]
  def handleError[A](ma: M[A])(recovery: errTy ⇒ M[A]): M[A] =  // TODO: refactor
    RM.rBind(ma)(r => r.fold(
               _     => RM.rPoint(r),
               error => recovery(error)
    ))
  def raiseError[A](errors: errTy) = RM.rPoint(Result.these[A](errors))

  trait ResultantMonadLaw extends MonadLaw with PlusLaw

  def resultantMonadLaw: ResultantMonadLaw = new ResultantMonadLaw {}
}
