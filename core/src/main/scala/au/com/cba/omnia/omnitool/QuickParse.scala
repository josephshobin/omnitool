package au.com.cba.omnia.omnitool

import scala.util.Try

import java.lang.Character._

import scalaz._, Scalaz._

sealed trait Parsed
case class ParsedLong(value: Long)     extends Parsed
case class ParsedDouble(value: Double) extends Parsed
case class ParsedString(value: String) extends Parsed

/**
  * A number parsing library that avoids the overhead of throwing/catching exceptions.
  */
object QuickParse {

  def long(value: String): Option[Long] = {
    def hasLongChars(s: String) = s.forall(c => isDigit(c) || c == '-')
    if (value != null && hasLongChars(value)) Try(value.toLong).toOption else None
  }

  def double(value: String): Option[Double] = {
    def hasDoubleChars(s: String) =
      s.forall(c => isDigit(c) || c == '.' || c == '-' || c == '+' || c == 'E' || c == 'e' || c == ' ' || c == '\t')
    if (value != null && hasDoubleChars(value)) Try(value.toDouble).toOption else None
  }

  def parse(value: String): Parsed =
    long(value).cata(
      ParsedLong.apply,
      double(value).cata(
        ParsedDouble.apply,
        ParsedString(value)
      )
    )
}
