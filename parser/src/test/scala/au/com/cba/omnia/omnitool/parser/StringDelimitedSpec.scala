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

import scalaz.Success

import au.com.cba.omnia.test.OmniaSpec

class StringDelimitedSpec extends OmniaSpec { def is = s2"""
String Delimited
================

Encoding a list of strings should:
  place the delimiter between the string values $delimited
  backslash escape any delimiter characters or backslashes present inside the strings $escapeDelimiter
  Encoding an empty list should decode to an empty list $emptyList
  Encoding a list of strings followed by decoding it should return the same value $encodeThenDecode
  Can encode and decode specific examples $specific

"""

  /** Helper to manually ensure a string has the delimiter character and backslashes present */
  def addDelimiters(delimiter: Char)(value: String) = (delimiter + '\\' + value + '\\' + delimiter)

  def delimited = prop { (values: List[String], delimiter: Char) => {
    val sanitizedValues = values.map(_.replace(delimiter, 'a').replace('\\', 'b'))
    StringDelimited.encode(sanitizedValues, delimiter) must beEqualTo(sanitizedValues.mkString(delimiter.toString))
  }}

  def escapeDelimiter = prop { (values: List[String], delimiter: Char) => {
    val sanitizedValues = values.map(_.replace(delimiter, 'a').replace('\\', 'b'))
    val valuesWithEscapingNeeded = sanitizedValues.map(str => "\\" + str + delimiter)
    StringDelimited.encode(valuesWithEscapingNeeded, delimiter) must beEqualTo(sanitizedValues.map(str => "\\\\" + str + s"\\$delimiter").mkString(delimiter.toString))
  }}

  def emptyList = prop { (delimiter: Char) =>
    val encoded = StringDelimited.encode(Nil, delimiter)
    encoded must beEqualTo("")
    StringDelimited.decode(encoded, delimiter)  must_== Success(List(""))
  }

  def encodeThenDecode = prop { (values: List[String], delimiter: Char) => (values.length > 0) ==> {
    val withDelimiters = values.map(addDelimiters(delimiter))
    StringDelimited.decode(StringDelimited.encode(List("a", "b", "c"), delimiter), delimiter)  must_== Success(List("a", "b", "c"))
    StringDelimited.decode(StringDelimited.encode(withDelimiters, delimiter), delimiter).toEither must beRight.like {
      case parsed: List[String] => {
        parsed must containTheSameElementsAs(withDelimiters)
        parsed must contain(allOf(withDelimiters: _*)).inOrder
      }
    }
  }}
  
  def nestedDecoding = prop { (values: List[List[String]]) => { (values.length > 0) && (values.forall(_.length > 0)) } ==> {
    val encodedInner = values.map(nested => StringDelimited.encode(nested, ':'))
    val encodedOuter = StringDelimited.encode(encodedInner, ',')
    
    val decodedOuter = StringDelimited.decode(encodedOuter, ',')
    decodedOuter must_== Success(encodedInner)
    decodedOuter.toOption.get.map(s => StringDelimited.decode(s, ':').toOption).flatten must beEqualTo(values)
  }}

  def specific = {
    val data = List(
      ("a\tb\tc", List("a", "b", "c")),
      ("a\tb\t",  List("a", "b", "")),
      ("a\t",     List("a", "")),
      ("\t",      List("", "")),
      ("\t",      List("", "")),
      ("",        List(""))
    )

    data.foreach { case (in, out) =>
      StringDelimited.decode(in, '\t')  must_== Success(out)
      StringDelimited.encode(out, '\t') must_== in
    }

    ok
  }
}
