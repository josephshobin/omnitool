package au.com.cba.omnia.omnitool

import org.scalacheck._, Arbitrary._, Gen._


import scalaz._, Scalaz._, Tag._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

class ResultSpec extends OmnitoolTest { def is = s2"""

Results
=======

Result[E, T] should
  obey the functor laws ${functor.laws[({type l[a] = Result[Int, a]})#l]}
  obey the bifunctor laws ${bifunctor.laws[Result]}
  obey the monad laws ${monad.laws[({type l[a] = Result[Int, a]})#l]}

`safeMap` should
  behave the same as map if the passed function doesn't throw exceptions $safeMapBehavesAsMap
  catch exceptions thrown by the passed function $safeMapCatchesExceptions

`collapse` should

"""

  def safeMapBehavesAsMap = prop { (r: Result[Int, Int], f: Int => Int) =>
    r.safeMap(f) must beEqualTo(r.map(f))
  }

  def safeMapCatchesExceptions = prop { (v: Int, ex: Exception) =>
    Result.Value(v).safeMap(_ => throw ex) must beEqualTo(Result.Exception(ex))
  }

  def valueGen[T](implicit gen: Arbitrary[T]): Gen[Result[Int, T]] =
    gen.arbitrary.map(Result.Value(_))

  def errorGen[T]: Gen[Result[Int, T]] =
    arbitrary[Int].map(Result.Error(_))

  def exceptionGen[T]: Gen[Result[Int, T]] =
    arbitrary[Exception].map(Result.Exception(_))

  implicit val results: Arbitrary[Result[Int, Int]] = Arbitrary(
    Gen.oneOf(valueGen[Int], errorGen, exceptionGen)
  )

  implicit val resultsFunction: Arbitrary[Result[Int, Int => Int]] = Arbitrary(
    Gen.oneOf(valueGen[Int => Int], errorGen[Int => Int], exceptionGen[Int => Int])
  )
}
