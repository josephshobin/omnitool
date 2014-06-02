package au.com.cba.omnia
package omnitool

import scalaz.{NonEmptyList, Validation}

object Validated {
  def safe[T](a: => T): Validated[T] =
    Validation.fromTryCatch(a).leftMap(exception => NonEmptyList(exception.toString))
}

case class RichValidated[T](validated: Validated[T]) {
  def addError(msg: String) = validated.leftMap(msg <:: _)
}
