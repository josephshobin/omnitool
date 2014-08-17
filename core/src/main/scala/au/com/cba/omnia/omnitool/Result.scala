package au.com.cba.omnia.omnitool

import scala.util.control.NonFatal

import scalaz._, Scalaz._


/**
 * A `Result[E, T]` represents either:
 *    - An `Error[E]` (where `E` is a specific data type)
 *    - An `Exception` (thrown by a Java library)
 *    - A `Value[T]` (if the function succeeded)
 *
 * To construct a `Result[E, T]`, either use one of the helper methods on the companion object or
 * use the datatype constructors on the companion object.
 *
 * When using a Result[E, T]:
 *    - The callee should return a bounded set of errors that the caller should handle (e.g. a
 *      sealed trait).
 *    - An exception returned by itself should be propagated upwards and represents an
 *      *exceptional* circumstance. It should not be used to implement logic.
 *      Specifically, it represents an error that can't or shouldn't be handled, or a defect.
 *      For example, if the application assumes a database connection is available then the lack
 *      of that should cause the program to exit (thus an exception is okay).
 *
 * A `Result[E, T]` should never be used to catch a `Throwable` exception.
 * This is to prevent it from catching unrecoverable exceptions (e.g. Out of Memory errors) or
 * other control exceptions (e.g. Scala control flow exceptions).  The set of non-recoverable
 * exceptions are those specified by [[scala.util.control.NonFatal]].
 *
 * NB. a function returning a result may or may not be a side-effecting operation
 *     (i.e. its use does not necessarily indicate a side-effect).
 *
 * @tparam E error type
 * @tparam T value type
 */
sealed abstract class Result[+E, +T] {
  import Result._

  /** Maps the value of the Result, but will NOT handle exceptions thrown by `f`. */
  def map[U](f: T => U): Result[E, U] = this match {
    case Value(value)  => Value(f(value))
    case Error(err)    => Error(err)
    case ex: Exception => ex
  }

  /** Maps the value of the Result and catches any exceptions the given function throws. */
  def safeMap[U](f: T => U): Result[E,U] = this match {
    case Value(value) =>
      try {
        Value(f(value))
      } catch {
        case NonFatal(ex: scala.Exception) => Exception(ex)
      }
    case Error(err)       => Error(err)
    case other: Exception => other
  }

  /** Flatmaps the value of the result, but will *NOT* handle exceptions thrown by `f`. */
  def flatMap[EE >: E, U](f: T => Result[EE, U]): Result[EE, U] = this match {
    case Value(value)  => f(value)
    case Error(err)    => Error(err)
    case ex: Exception => ex
  }

  /** Maps across the error side of the result. */
  def errorMap[EE](f: E => EE): Result[EE, T] =
    bifunctor.bimap(this)(f, identity)

  /**
   * Folds across the result. The first function (`fromValue`) executes in the case of a value,
   * the second function (`fromError`) executes in the case of an error, and the third function
   * (`fromException`) executes in the case of an exception.
   */
  def fold[U](
    fromValue: T => U,
    fromError: E => U,
    fromException: scala.Exception => U
  ): U = this match {
    case Value(value)  => fromValue(value)
    case Error(err)    => fromError(err)
    case Exception(ex) => fromException(ex)
  }

  /**
   * Returns the value of a result, or throws an exception if the result is an error, or
   * rethrows an exception that the result holds.
   */
  def unsafeGet(): T = this match {
    case Value(value)   => value
    case e@Error(_)     => throw new ResultException(e)
    case e@Exception(_) => throw new ResultException(e)
  }

  /**
   * Recovers from an exception using the partial function `f`. If the result contains an
   * exception, and `f` is defined for the `scala.Exception` that it holds, then `f` is applied.
   * Errors and values are not transformed. This function does *NOT* recover from an error.
   */
  def recoverFromException[EE >: E, U >: T](
    f: PartialFunction[scala.Exception, Result[EE,U]]
  ): Result[EE, U] = this match {
    case Exception(ex) => f.applyOrElse(ex, Result.Exception)
    case errorOrValue  => errorOrValue
  }

  /**
   * Converts this result to an `Option`. A value is converted to `Some`, while errors and
   * exceptions are both converted to `None`.
   */
  def toOption: Option[T] = this match {
    case Value(value) => Some(value)
    case Error(_)     => None
    case Exception(_) => None
  }

  /**
   * Converts this result to a `ValidationNel`. A value is converted to a success, while errors
   * and exceptions are both converted to failures.
   */
  def toValidated: ValidationNel[String, T] = this match {
    case Value(value)   => value.successNel
    case e@Error(_)     => e.toString.failNel
    case e@Exception(_) => e.toString.failNel
  }

  /** Tests if this result is a value. */
  def isValue: Boolean = this match {
    case Value(value) => true
    case _            => false
  }

  /** Tests if this result is an error or an exception. */
  def isFailure: Boolean = this match {
    case Value(value) => false
    case _            => true
  }
}

object Result {

  /**
   * Represents a value result of type `T`. This instance normally contains the successful value
   * of an operation that returns a result.
   */
  final case class Value[T](value: T) extends Result[Nothing, T]

  /**
   * Represents an error result of type `E`. This instance normally contains an expected error
   * produced by an operation that returns a result.
   */
  final case class Error[E](error: E) extends Result[E, Nothing]

  /**
   * Represents an exception (of type `scala.Exception`) that occurs as an exceptional circumstance
   * of some operation that returns a result. For example, this may represent an exception thrown
   * by a Java library.
   */
  final case class Exception(exception: scala.Exception) extends Result[Nothing, Nothing]

  /** Helper function to create an value instance of a result. */
  def value[T](value: T): Result[Nothing, T] = Result.Value(value)

  /** Helper function to create an error instance of a result. */
  def error[E](error: E): Result[E, Nothing] = Result.Error(error)

  /** Helper function to create an exception instance of a result. */
  def exception(ex: scala.Exception): Result[Nothing, Nothing] = Result.Exception(ex)

  /**
   * Calls function `f`, capturing any non-fatal exceptions it throws as exception instances of
   * the result.
   */
  def fromTryCatch[T](f: => T): Result[Nothing, T] = try {
    Value(f)
  } catch {
    case NonFatal(ex: scala.Exception) => Exception(ex)
  }

  /**
   * Calls function `f` with a resource of type `A`. The resource is first created by the `init`
   * function. After `f` has been executed, the resource is cleaned up by the `cleanup`
   * function in a `finally` block. Any non-fatal exceptions thrown during this process are
   * captured as exception instances of the result.
   */
  def fromTryCatchFinally[A, B](init: => A)(f: A => B)(cleanup: A => Unit): Result[Nothing, B] =
    try {
      val resource = init
      try {
        Value(f(resource))
      } catch {
        case NonFatal(ex: scala.Exception) => Exception(ex)
      } finally {
        cleanup(resource)
      }
    } catch {
      case NonFatal(ex: scala.Exception) => Exception(ex)
    }

  /** Exception that is thrown by the result (for example, by [[Result.unsafeGet]]). */
  final case class ResultException[E](result: Result[E, Nothing]) extends scala.Exception {
    override def getMessage: String = result.toString

    override def getCause: Throwable = result.fold[scala.Exception](_ => null, _ => null, ex => ex)

    override def toString: String = result match {
      case Value(value)  => s"Value: $value"
      case Error(error)  => s"Error: $error"
      case Exception(ex) => s"Threw Exception: $ex"
    }
  }

  /** Monad type class instance. */
  implicit def monad[E] = new Monad[({type l[a] = Result[E, a]})#l] {
    override def bind[A, B](fa: Result[E, A])(f: (A) => Result[E, B]) =
      fa.flatMap(f)

    override def point[A](a: => A) = Value(a): Result[E, A]
  }

  /** Equal type class instance. */
  implicit def equal[A, B] = Equal.equalA[Result[A, B]]

  /** Bifunctor type class instance. */
  implicit def bifunctor = new Bifunctor[Result] {
    override def bimap[A, B, C, D](result: Result[A, B])(f: A => C, g: B => D): Result[C, D] =
      result match {
        case Value(value)  => Value(g(value))
        case Error(err)    => Error(f(err))
        case Exception(ex) => Exception(ex)
      }
  }
}
