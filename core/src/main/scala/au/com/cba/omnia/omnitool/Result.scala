package au.com.cba.omnia.omnitool

import scalaz._, Scalaz._

/**
 * A Result[E, T] represents either:
 *    - An Error (a specific data type)
 *    - An Exception (thrown by a Java library)
 *    - A Value (if the function succeeded)
 *
 * To use a Result[E, T], either use one of the helper methods on the companion object or
 * use the datatype constructors on the companion object.
 *
 * When using a Result[E, T]:
 *    - The callee should return a bounded set of errors that the caller should handle (e.g. a sealed trait).
 *    - An exception returned by itself should be propagated upwards and represents an *exceptional* circumstance.
 *      It should not be used to implement logic.
 *      Specifically it represents an error that can't or shouldn't be handled or a defect.
 *      For example, if the application assumes a database connection is available then the lack of that should
 *      cause the program to exit (thus an exception is okay).
 *
 * A Result[E, T] should never be used to catch a `Throwable` exception.
 * This is to prevent it from catching unrecoverable exceptions (e.g. Out of Memory errors) or
 * other control exceptions (e.g. Scala control flow exceptions).
 *
 * NB. a function returning a result may or may not be a side-effecting operation
 *     (e.g. its use does not necessarily indicate a side-effect).
 *
 * @tparam E error type
 * @tparam T value type
 */
sealed abstract class Result[+E, +T] {
  import Result._

  /**
   * Maps the value of the Result but will NOT handle exceptions thrown by `f`.
   */
  def map[U](f: T => U): Result[E, U] = this match {
    case Value(value)                 => Value(f(value))
    case other: Exception        => other
    case Error(err)                   => Error(err)
  }

  /**
   * Maps the value of the Result and catches any exceptions the given function throws.
   */
  def safeMap[U](f: T => U): Result[E,U] = this match {
    case Value(value) =>
      try {
        Value(f(value))
      } catch {
        case ex: scala.Exception => Exception(ex)
      }
    case Error(err)       => Error(err)
    case other: Exception => other
  }

  /**
   * Flatmaps the value of the result but will NOT handle exceptions thrown by f
   */
  def flatMap[EE >: E, U](f: T => Result[EE, U]): Result[EE, U] = this match {
    case Value(value)     => f(value)
    case Error(err)       => Error(err)
    case other: Exception => other
  }

  /**
   * Maps across the error side of the result.
   */
  def errorMap[EE](f: E => EE): Result[EE, T] =
    bifunctor.bimap(this)(f, identity)

  /**
   * Folds across the result where the first function executes in the case of
   * an error, exception or error and exception; and the second function
   * executes in the case of the a value.
   */
  def fold[U](
              fromValue: T => U,
              fromError: E => U,
              fromException: scala.Exception => U
             ) = this match {
    case Value(value)        => fromValue(value)
    case Error(err)          => fromError(err)
    case Exception(ex)       => fromException(ex)
  }

  /**
   * Returns the value of the result or throws an exception if the result is an
   * error or rethrows the exception (that the result holds).
   */
  def throwIfError() = this match {
    case Value(value)     => value
    case e@Error(err)     => throw new ResultException(e)
    case e@Exception(ex)  => throw new ResultException(e)
  }

  def recoverFromException[EE >: E, U >: T](f: PartialFunction[scala.Exception, Result[EE,U]]): Result[EE, U] = this match {
    case Exception(ex)  => f.applyOrElse(ex, Result.Exception)
    // NB. does not recover from an error.
    case errorOrValue   => errorOrValue
  }

  def toOption: Option[T] = this match {
    case Value(value)               => Some(value)
    case Error(_)                   => None
    case Exception(_)          => None
  }

  def toValidated: ValidationNel[String, T] = this match {
    case Value(value)                   => value.successNel
    case e@Error(err)                   => e.toString.failNel
    case e@Exception(ex)           => e.toString.failNel
  }

  def isValue = this match {
    case Value(value)                 => true
    case _                            => false
  }

  def isFailure = this match {
    case Value(value)                 => false
    case _                            => true
  }
}

object Result {

  final case class Value[T](value: T) extends Result[Nothing, T]

  final case class Error[E](error: E) extends Result[E, Nothing]

  final case class Exception(exception: scala.Exception) extends Result[Nothing, Nothing]

  // Helper functions for creating instances.
  def value[T](value: T): Result[Nothing, T] = Result.Value(value)

  def error[E](error: E): Result[E, Nothing] = Result.Error(error)

  def exception(ex: scala.Exception): Result[Nothing, Nothing] = Result.Exception(ex)

  def fromTryCatch[T](f: => T): Result[Nothing, T] = try {
    Value(f)
  } catch {
    case ex: scala.Exception => Exception(ex)
  }

  def fromTryCatchFinally[A, B](init: => A)(f: A => B)(cleanup: A => Unit): Result[Nothing, B] =
    try {
      val resource = init
      try {
        Value(f(resource))
      } catch {
        case ex: scala.Exception => Exception(ex)
      } finally {
        cleanup(resource)
      }
    } catch {
      case ex: scala.Exception => Exception(ex)
    }

  final case class ResultException[E](result: Result[E, Nothing]) extends scala.Exception {
    override def getMessage() = result.toString()

    override def getCause() = result.fold[scala.Exception](_ => null, _ => null, ex => ex)

    override def toString() = result match {
      case Value(value)   => s"Value: $value"
      case Error(error)   => s"Error: $error"
      case Exception(ex)  => s"Threw Exception: $ex"
    }
  }

  // Type classes
  implicit def monad[E] = new Monad[({type l[a] = Result[E, a]})#l] {
    override def bind[A, B](fa: Result[E, A])(f: (A) => Result[E, B]) =
      fa.flatMap(f)

    override def point[A](a: => A) = Value(a): Result[E, A]
  }

  implicit def equal[A, B] = Equal.equalA[Result[A, B]]

  implicit def bifunctor = new Bifunctor[Result] {
    override def bimap[A, B, C, D](result: Result[A, B])(f: A => C, g: B => D): Result[C, D] = result match {
      case Value(value)   => Value(g(value))
      case Error(err)     => Error(f(err))
      case Exception(ex)  => Exception(ex)
    }
  }
}
