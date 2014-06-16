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
