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

import scala.util.Try

import scalaz.{Monad, Plus}
import scalaz.syntax.{MonadSyntax, PlusSyntax}

import ResultantMonadSyntax._

trait MonadAndPlus[M[_]] extends Monad[M] with Plus[M]
/**
  * Convenient operations that you can do on the companion of a [[ResultantMonad]].
  *
  * The companion object should extend this class to avoid clashing with Scalaz implicits.
  */
trait ResultantOps[M[_]] extends MonadSyntax[M] with PlusSyntax[M] {
  def ResultRel: RelMonad[Result, M]
  def F: MonadAndPlus[M] = ResultRel          // For MonadSyntax and PlusSyntax

  @deprecated("Use ResultRel instead")
  def monad = ResultRel

  /** Build an operation from a value. The resultant DB operation will not throw an exception. */
  def value[A](v: => A): M[A] =
    monad.point(v)

  /** Builds an operation from a [[Result]]. */
  def result[A](v: => Result[A]): M[A] =
    monad.rPoint(v)

  /** Builds an operation from an `Either[String, A]`. */
  def eitherFail[A](v: => Either[String, A]): M[A] =
    result(Result.eitherFail(v))

  /** Builds an operation from an `Either[Throwable, A]`. */
  def eitherException[A](v: => Either[Throwable, A]): M[A] =
    result(Result.eitherException(v))

  /** Builds an operation from an `Either[(String, Throwable), A]`. */
  def eitherError[A](v: => Either[(String, Throwable), A]): M[A] =
    result(Result.eitherError(v))

  /** Builds an operation from a `Try`. */
  def fromTry[A](v: => Try[A]): M[A] =
    result(Result.fromTry(v))

  /** Build a failed operation from the specified message. */
  def fail[A](message: String): M[A] =
    monad.rPoint(Result.fail(message))

  /** Build a failed M operation from the specified exception. */
  def exception[A](t: Throwable): M[A] =
    monad.rPoint(Result.exception(t))

  /** Build a failed M operation from the specified exception and message. */
  def error[A](message: String, t: Throwable): M[A] =
    monad.rPoint(Result.error(message, t))

  /**
    * Fails if condition is not met
    *
    * Provided instead of `scalaz.MonadPlus` typeclass, as M does not
    * quite meet the required laws.
    */
  def guard(ok: Boolean, message: String): M[Unit] =
    monad.rPoint(Result.guard(ok, message))

  /**
    * Fails if condition is met
    *
    * Provided instead of `scalaz.MonadPLus` typeclass, as M does not
    * quite meet the required laws.
    */
  def prevent(fail: Boolean, message: String): M[Unit] =
    monad.rPoint(Result.prevent(fail, message))

  /**
    * Ensures a M operation returning a boolean success flag fails if unsuccessful
    *
    * Provided instead of `scalaz.MonadPLus` typeclass, as M does not
    * quite meet the required laws.
    */
  def mandatory(action: M[Boolean], message: String): M[Unit] =
    action flatMap (guard(_, message))

  /**
    * Ensures a M operation returning a boolean success flag fails if successful
    *
    * Provided instead of `scalaz.MonadPLus` typeclass, as M does not
    * quite meet the required laws.
    */
  def forbidden(action: M[Boolean], message: String): M[Unit] =
    action flatMap (prevent(_, message))
}
