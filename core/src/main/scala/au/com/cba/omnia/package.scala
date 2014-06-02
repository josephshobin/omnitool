package au.com.cba.omnia

import scalaz.ValidationNel

package object omnitool {
  type Validated[T] = ValidationNel[String, T]

  implicit def ValidatedToRichValidated[T](v: Validated[T]) = RichValidated[T](v)
}
