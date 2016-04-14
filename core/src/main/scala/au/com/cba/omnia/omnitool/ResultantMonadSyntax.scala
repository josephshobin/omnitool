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

import scalaz.{MonadPlus, Monad, Plus, MonadError}
import scalaz.\&/._

import scalaz.\&/.These
import scalaz.syntax.{ToPlusOps, ToMonadOps}
import scalaz.syntax.monad._


/** Convenient operations that you can do on a [[ResultantMonad]]. */
final class ResultantMonadOps[M[_], A](val self: M[A])(implicit val M: RelMonad[Result, M]) {
  import ResultantMonadSyntax._

  /** Chain a context free result (i.e. requires no configuration) to this operation. */
  def andThen[B](f: A => Result[B]): M[B] =    // This could be generalised in a RelMonadOps for [R[_]: Monad]
    M.rBind(self)(a => M.rPoint(a.flatMap(f)))

  /** Maps across the result. */
  def rMap[B](f: Result[A] => Result[B]): M[B] =  // This could be generalised via a RelMonadOps
    M.rBind(self)(r => M.rPoint(f(r)))

  /** FlatMaps across the result. */
  def rFlatMap[B](f: Result[A] => M[B]): M[B] =   // This could be generalised via a RelMonadOps
    M.rBind(self)(f)

  /**
    * Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
    *
    * Returns the error of `self` iff both `self` and `alternative` fail.
    */
  def or(alternative: => M[A]): M[A] =
    M.plus(self, alternative)

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    *
    * NB: This discards any existing message.
    */
  def setMessage(message: String): M[A] =
    M.rMap(self)(_.setMessage(message))

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    *
    * The new message is prepended to any existing message.
    */
  def addMessage(message: String, separator: String = ": "): M[A] =
    M.rMap(self)(_.addMessage(message, separator))

  /** Recovers from an error. */
  def recoverWith(recovery: PartialFunction[These[String, Throwable], M[A]]): M[A] =
    M.rBind(self)(r => r.fold(
      _     => M.rPoint(r),
      error => recovery.applyOrElse[These[String, Throwable], M[A]](error, _ => M.rPoint(r))
    ))

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def onException[B](action: => M[B]): M[A] =
    recoverWith { case e => M.rBind(action)(_ => M.rPoint(Result.these(e))) }

  /**
    * Ensures that the provided action is always run regardless of if `this` was successful.
    * Generalizes "finally".
    *
    * If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
    * the result of `self` is returned.
    */
  def ensure[B](sequel: => M[B]): M[A] = for {
    r <- onException(sequel)
    _ <- sequel
  } yield r

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an error.
    *
    * All errors are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => M[B])(during: A => M[C]): M[C] = for {
    a <- self
    r <- during(a) ensure after(a)
  } yield r
}

/** Pimps a [[ResultantMonad]] to have access to the functions in [[ResultantMonadOps]].
  * Also pimps RelMonad[Result, _] instances via RichResultantMonad.
  * The usual use of this is to mix it into the companion object to ensure the implicit resolution
  * priority does not clash with Scalaz.
  */
trait ToResultantMonadOps {
  /** Pimps a [[ResultantMonad]] to have access to the functions in [[ResultantMonadOps]]. */
  implicit def ToResultantMonadOps[M[_], A](v: M[A])(implicit M0: RelMonad[Result, M]): ResultantMonadOps[M, A] =
    new ResultantMonadOps[M, A](v)

  // Sometimes RelMonad instances are explict, sometimes implicit, so we need both of the following.
  implicit def toImplicitRichResultantMonad[M[_]](RM: RelMonad[Result, M]): RichResultantMonad[M] =
    new RichResultantMonad[M](RM)
  implicit def toRichResultantMonad[M[_]](implicit RM: RelMonad[Result, M]): RichResultantMonad[M] =
    new RichResultantMonad[M](RM)
}

/** Pimps a [[ResultantMonad]] to have access to the functions in [[ResultantMonadOps]]. */
object ResultantMonadSyntax extends ToResultantMonadOps

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
