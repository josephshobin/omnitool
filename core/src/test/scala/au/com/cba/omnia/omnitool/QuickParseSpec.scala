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

import org.scalacheck.{Prop, Arbitrary, Gen}

class QuickParseSpec extends OmnitoolTest { def is = s2"""

QuickParse
==========

When parsing Long values:
  strings containing valid longs should parse correctly $validLong
  strings without valid longs shouldn't parse $invalidLong
  prepended plus characters are not permitted $prependedPlusLong
  leading and trailing space or tab is not OK $envelopingSpaceLong
  null values should return None $longNull

When parsing Double values:
  strings containing valid doubles should parse correctly $validDouble
  strings without valid doubles shouldn't parse $invalidDouble
  both 'e' and 'E' are permitted exponent characters $exponents
  prepended plus characters are permitted $prependedPlusDouble
  leading and trailing space or tab is OK $envelopingSpaceDouble
  null values should return None $doubleNull

Arbitrary strings:
  must parse correctly $parse

"""

  def parse = {
    val long   = 12
    val double = 1.0
    val string = "abbcd"

    QuickParse.parse(long.toString)   must_== ParsedLong(long)
    QuickParse.parse(double.toString) must_== ParsedDouble(double)
    QuickParse.parse(string)          must_== ParsedString(string)
  }

  def validLong = prop { (l: Long) =>
    QuickParse.long(l.toString) must beSome(l)
  }

  def invalidLong = prop { (s: String) => Try(s.toLong).isFailure ==>
    (QuickParse.long(s) must beNone)
  }

  def prependedPlusLong = prop { (l: Long) => (l > 0) ==>
    (QuickParse.long(s"+${l.toString}") must beNone)
  }

  def envelopingSpaceLong = Prop.forAll (genEnvelopingSpace[Long](minSpace=1)) {
    case Envelope(long, string) => QuickParse.long(string) must beNone
  }

  def longNull = {
    QuickParse.long(null) must beNone
  }

  def validDouble = prop { (d: Double) =>
    QuickParse.double(d.toString) must beSome(d)
  }

  def invalidDouble = prop { (s: String) => Try(s.toDouble).isFailure ==>
    (QuickParse.double(s) must beNone)
  }

  def exponents = prop { (d: Double) =>
    val upper = d.toString.toUpperCase
    val lower = d.toString.toLowerCase
    upper.contains('E') ==> {
      val upperParse = QuickParse.double(upper) must beSome(d)
      val lowerParse = QuickParse.double(lower) must beSome(d)
      upperParse && lowerParse
    }
  }

  def prependedPlusDouble = prop { (d: Double) => (d > 0.0) ==>
    (QuickParse.double(s"+${d.toString}") must beSome(d))
  }

  def envelopingSpaceDouble = Prop.forAll (genEnvelopingSpace[Double](minSpace=0)) {
    case Envelope(double, string) => QuickParse.double(string) must beEqualTo(Some(double))
  }

  def doubleNull = {
    QuickParse.double(null) must beNone
  }

  // Represents a value, along with a string form of that value, typically enveloped in whitespace
  private case class Envelope[A](value: A, string: String) {
    override def toString: String = s"""Envelope($value, "$string")"""
  }

  // Creates a generator which forms a string version of a type with both leading and trailing spaces and tabs
  private def genEnvelopingSpace[A](minSpace: Int)(implicit arb: Arbitrary[A]): Gen[Envelope[A]] = {
    val maxSpace = 10
    for {
      leading  <- genSpacesAndTabs(minSpace, maxSpace)
      trailing <- genSpacesAndTabs(minSpace, maxSpace)
      a        <- arb.arbitrary
    } yield Envelope(a, s"$leading${a.toString}$trailing")
  }

  // Creates a generator which forms a string with spaces and tabs
  private def genSpacesAndTabs(minLength: Int, maxLength: Int): Gen[String] = {
    def updateMulti(s: String, indices: List[Int], elem: Char): String =
      indices.foldLeft(s){ case (string, index) => string.updated(index, elem) }
    for {
      spaces     <- for { nChars <- Gen.choose(minLength, maxLength) } yield (" " * nChars)
      tabIndices <- Gen.containerOf[List,Int]( Gen.choose(0, spaces.length - 1) )
    } yield updateMulti(spaces, tabIndices, '\t')
  }

}
