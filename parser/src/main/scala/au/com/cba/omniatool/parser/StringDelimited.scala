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

package au.com.cba.omnia.omnitool.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharArrayReader

import scalaz._, Scalaz._

import au.com.cba.omnia.omnitool.Validated

/**
  * A helper object for encoding a list of strings as a single delimited string.
  * This is still a work in progress.
  */
object StringDelimited {
  case class BackslashParser(delimiterChar: Char) extends Parsers {
    type Elem = Char

    private val backslash = accept('\\')
    private val delimiter = accept(delimiterChar)

    private val escapedBackslash = backslash <~ backslash
    private val escapedDelimiter = backslash ~> delimiter
    private val delimiterAsSeperator = delimiter.map(_ => (fst: List[String], snd: List[String]) => fst ++ snd)

    /** Parser that accepts anything but a delimiter or backslash */
    private val anythingElse = acceptIf(c => c != delimiterChar)(err => s"Unexpected (should be a regular character) $err")

    /** Blocks of escaped values separated by the delimiter. */
    val unescape = ((escapedBackslash | escapedDelimiter | anythingElse)*).map(str => List(str.mkString)).*(delimiterAsSeperator)
  }

  /**
    * Encodes list of strings as a delimiter delimited string.
    * Nested delimiters are escaped using \ and all \ are escaped using \ as well.
    */
  def encode(values: Seq[String], delimiter: Char): String =
    values.map(_.replace("\\", "\\\\").replace(delimiter.toString, s"\\$delimiter")).mkString(delimiter.toString)

  /**
    * Decode strings separated by the delimiter character.
    * **Requires all \ to have been escaped.**
    * Supports nested delimiter characters by escaping them with \.
    */
  def decode(input: String, delimiter: Char): Validated[List[String]] = {
    val parser = BackslashParser(delimiter)

    parser.unescape(new CharArrayReader(input.toCharArray)) match {
      case parser.Success(result, xs) => result.successNel
      case parser.Error(message, _)   => message.failureNel
      case parser.Failure(message, _) => message.failureNel
    }
  }
}
