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

import scalaz.\&/.These
import scalaz.syntax.{ToPlusOps, ToMonadOps}
import scalaz.syntax.monad._

/** Convenient operations that you can do on a [[ResultantMonad]]. */
final class ResultantMonadOps[M[_], A](val self: M[A])(implicit val M: ResultantMonad[M]) {
  import ResultantMonadSyntax._, Result.{ResultMonad => R}

  /** Chain a context free result (i.e. requires no configuration) to this operation. */
  def andThen[B](f: A => Result[B]): M[B] =
    M.rBind(self)(ra => R.point(M.rPoint(ra.flatMap(f))))

  /** Maps across the result. */
  def rMap[B](f: Result[A] => Result[B]): M[B] =
    M.rBind(self)(r => R.point(M.rPoint(f(r))))

  /** FlatMaps across the result. */
  def rFlatMap[B](f: Result[A] => M[B]): M[B] =
    M.rBind(self)(r => R.point(f(r)))

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
    rMap[A](_.setMessage(message))

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    *
    * The new message is prepended to any existing message.
    */
  def addMessage(message: String, separator: String = ": "): M[A] =
    rMap[A](_.addMessage(message, separator))

  /** Recovers from an error. */
  def recoverWith(recovery: PartialFunction[These[String, Throwable], M[A]]): M[A] =
    rFlatMap(r => r.fold(
      _     => M.rPoint(r),
      error => recovery.applyOrElse[These[String, Throwable], M[A]](error, _ => M.rPoint(r))
    ))

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def onException[B](action: M[B]): M[A] =
    recoverWith { case e => action.rFlatMap(_ => M.rPoint(Result.these(e))) }

  /**
    * Ensures that the provided action is always run regardless of if `this` was successful.
    * Generalizes "finally".
    *
    * If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
    * the result of `self` is returned.
    */
  def ensure[B](sequel: M[B]): M[A] = for {
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
  *
  * The usual use of this is to mix it into the companion object to ensure the implicit resolution
  * priority does not clash with Scalaz.
  */
trait ToResultantMonadOps extends ToMonadOps with ToPlusOps {
  /** Pimps a [[ResultantMonad]] to have access to the functions in [[ResultantMonadOps]]. */
  implicit def ToResultantMonadOps[M[_], A](v: M[A])(implicit M0: ResultantMonad[M]): ResultantMonadOps[M, A] =
    new ResultantMonadOps[M, A](v)
}

/** Pimps a [[ResultantMonad]] to have access to the functions in [[ResultantMonadOps]]. */
object ResultantMonadSyntax extends ToResultantMonadOps
