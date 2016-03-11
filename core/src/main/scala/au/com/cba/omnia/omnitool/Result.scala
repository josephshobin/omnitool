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
import scala.util.{Try, Success, Failure}

import scalaz._, Scalaz._, \&/._


/**
  * A data-type useful for representing the result of a possibly failing operation.
  * 
  * The Error result of an operation represents an initial error (either an error message or an
  * exception) with the option to add more details in the form of error messages.
  * 
  * The reason this exists as an alternative to using the more general
  * `Either`, `\/`, `Validation`, `Try` data-types is that we want to
  * gain better inference (we gain most of this by specialising the
  * error case), better composition (compared with `Try`, we gain most
  * of this by not mixing side-effects with map/flat) and better
  * library support by adding specific combinators for our given
  * case.
  */
sealed trait Result[A] {
  /**
    * `Catamorphism` for result. Concise data deconstruction that can be used as an alternative to
    * pattern matching, providing stronger coverage checks.
    */
  @inline final def fold[X](
    ok: A => X,
    error: These[String, Throwable] => X
  ): X = this match {
    case Ok(value)    => ok(value)
    case Error(these) => error(these)
  }

  /**
    * `Catamorphism` for result and the underlying `These`. Concise data deconstruction that can be
    * used as an alternative to pattern matching, providing stronger coverage checks.
    */
  @inline final def foldAll[X](
    ok: A => X,
    fail: String => X,
    exception: Throwable => X,
    both: (String, Throwable) => X
  ): X = fold(ok, _ match {
    case This(s)    => fail(s)
    case That(e)    => exception(e)
    case Both(s, e) => both(s, e)
  })

  /** `Catamorphism` for the sucess case or the error message. */
  @inline final def foldMessage[X](
    ok: A => X,
    error: String => X
  ): X = fold(ok, {
    case This(s)    => error(s)
    case That(ex)   => error(ex.getMessage)
    case Both(s, _) => error(s)
  })

  /** Map across the success case. */
  def map[B](f: A => B): Result[B] =
    flatMap(f andThen Result.ok)

  /** Bind through the success case, this is useful to chain potentially failing operations. */
  def flatMap[B](f: A => Result[B]): Result[B] =
    fold(f, Result.these)

  /** Convert to scalaz.\/, this is useful to access its rich library. */
  def toDisjunction: These[String, Throwable] \/ A =
    fold(_.right, _.left)

  /** Convert to scala.Either, this is useful for interop. */
  def toEither: Either[These[String, Throwable], A] =
    toDisjunction.toEither

  /** Convert to scala.Option, this is useful as it is common to not care how something fails. */
  def toOption: Option[A] =
    toDisjunction.toOption

  /** Convert the error case to an scala.Option. */
  def toError: Option[These[String, Throwable]] =
    toDisjunction.swap.toOption

  /** In the success case, get the value, otherwise return `els`, useful for defauling in error case. */
  def getOrElse(els: => A): A =
    toOption.getOrElse(els)

  /** Take the first successful result. Useful for chaining optional operations. */
  def or(other: => Result[A]): Result[A] =
    fold(Result.ok, _ => other)

  /** Alias for `or`. Provides nice syntax: `Result.fail("bad") ||| Result.ok(10)` */
  def |||(other: => Result[A]): Result[A] =
    or(other)

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    * 
    * NB: This discards any existing message.
    */
  def setMessage(message: String): Result[A] =
    foldAll(
      r      => Result.ok(r),
      _      => Result.fail(message),
      e      => Result.error(message, e),
      (_, e) => Result.error(message, e)
    )

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    * 
    * The new message is prepended to any existing message.
    */
  def addMessage(message: String, separator: String = ": "): Result[A] =
    foldAll(
      r      => Result.ok(r),
      m      => Result.fail(s"${message}${separator}$m"),
      e      => Result.error(message, e),
      (m, e) => Result.error(s"${message}${separator}$m", e)
    )
}

/**
  * Successful `Result`, public so it can be used for pattern matching, prefer Result.ok for
  * construction.
  */
case class Ok[A](value: A) extends Result[A]

/**
  * Failing `Result`, public so it can be used for pattern matching, prefer
  * Result.{these,error,fail,exception} for construction.
  */
case class Error[A](error: These[String, Throwable]) extends Result[A]

object Result {
  /** Exception safe `Result` creation. */
  def safe[A](thunk: => A): Result[A] =
    try ok(thunk) catch { case NonFatal(t) => exception(t) }

  /**
    * Smart constructor for a successful `Result`. Provides better inference then direct use of
    * constructor. */
  def ok[A](value: A): Result[A] =
    Ok[A](value)

  /** Smart constructor for a failing `Result` built from a `These`. */
  def these[A](e: These[String, Throwable]): Result[A] =
    Error(e)

  /** Smart constructor for a failing case with a message and an exception. */
  def error[A](message: String, t: Throwable): Result[A] =
    these(Both(message, t))

  /** Smart constructor for a failing case with only a message. */
  def fail[A](message: String): Result[A] =
    these(This(message))

  /** Smart constructor for a failing case with only an exception. */
  def exception[A](t: Throwable): Result[A] =
    these(That(t))

  /** Smart constructor for converting from an `Either[String, A]`. */
  def eitherFail[A](value: Either[String, A]): Result[A] = value match {
    case Left(e)  => fail(e)
    case Right(v) => ok(v)
  }

  /** Smart constructor for converting from an `Either[Throwable, A]`. */
  def eitherException[A](value: Either[Throwable, A]): Result[A] = value match {
    case Left(e)  => exception(e)
    case Right(v) => ok(v)
  }

  /** Smart constructor for converting from an `Either[(String, Throwable), A]`. */
  def eitherError[A](value: Either[(String, Throwable), A]): Result[A] = value match {
    case Left((err, exc)) => error(err, exc)
    case Right(v)         => ok(v)
  }

  /** Smart constructor for converting from a `Try[A]`. */
  def fromTry[A](value: Try[A]): Result[A] = value match {
    case Failure(e) => exception(e)
    case Success(v) => ok(v)
  }

  /**
    * Fails if condition is not met
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as Hdfs does not
    * quite meet the required laws.
    */
  def guard(ok: Boolean, message: String): Result[Unit] =
    if (ok) Result.ok[Unit](()) else fail[Unit](message)

  /**
    * Ensures a Result operation returning a boolean success flag fails if unsuccessfull
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as Result does not
    * quite meet the required laws.
    */
  def mandatory(result: Result[Boolean], message: String): Result[Unit] =
    result.flatMap(guard(_, message))

  /**
    * Ensures a Result operation returning a boolean success flag fails if succeesfull
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as Result does not
    * quite meet the required laws.
    */
  def forbidden(result: Result[Boolean], message: String): Result[Unit] =
    result.flatMap(prevent(_, message))

  /**
    * Fails if condition is met
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as Hdfs does not
    * quite meet the required laws.
    */
  def prevent(fail: Boolean, message: String): Result[Unit] =
    guard(!fail, message)

  /** scalaz Monad instance for Result. */
  implicit def ResultMonad: Monad[Result] = new Monad[Result] {
    def point[A](v: => A) = ok(v)
    def bind[A, B](a: Result[A])(f: A => Result[B]) = a flatMap f
  }

  implicit def ResultRelResult: RelMonad[Result, Result] = new RelMonad.SelfR[Result]

  /** scalaz Equal instance for Result. */
  implicit def ResultEqual[A : Equal]: Equal[Result[A]] = {
    implicit def ThrowableEqual = Equal.equalA[Throwable]
    implicitly[Equal[These[String, Throwable] \/ A]].contramap(_.toDisjunction)
  }

  /** scalaz Plus instance for Result. */
  implicit def ResultPlus: Plus[Result] = new Plus[Result] {
    def plus[A](a: Result[A], b: => Result[A]): Result[A] =
      a or b
  }
}
