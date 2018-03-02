//   Copyright 2015 Commonwealth Bank of Australia
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

package au.com.cba.omnia.omnitool.test

import org.specs2.matcher.{BeTypedEqualTo, Expectable, Matcher, MatchResult}

import au.com.cba.omnia.omnitool.Result

/** Matchers for Result. */
object ResultMatchers {
  /** Checks that the result succeeded with the specified value. */
  def beOk[A](a: A): Matcher[Result[A]] = new BeTypedEqualTo(Result.ok(a))

  /** Checks that the result failed with the specified error message. */
  def beFail[A](error: String): Matcher[Result[A]] = new BeTypedEqualTo(Result.fail(error))

  /** Checks that the result failed with an exceptation that maches the provided check. */
  def beExceptionLike[A](check: PartialFunction[Throwable, MatchResult[_]]): Matcher[Result[A]] = new Matcher[Result[A]]{
    def apply[S <: Result[A]](actual: Expectable[S]) = {
      actual.value.foldAll(
        _  => result(false, "", s"${actual.description} is not a Result failure with exception", actual),
        _  => result(false, "", s"${actual.description} is not a Result failure with exception", actual),
        ex => check.andThen(mr => result(mr.toResult, actual)).applyOrElse(
          ex,
          { t: Throwable => result(false, "", s"$ex is not the expected exception", actual) }
        ),
        (_, _) => result(false, "", s"${actual.description} is not a Result failure with exception", actual)
      )
    }
  }

  /** Checks that the result failed with an error and exceptation that maches the provided check. */
  def beErrorLike[A](f: PartialFunction[(String, Throwable), MatchResult[_]]): Matcher[Result[A]] = new Matcher[Result[A]]{
    def apply[S <: Result[A]](actual: Expectable[S]) = {
      actual.value.foldAll(
        _         => result(false, "", s"${actual.description} is not a Result error", actual),
        _         => result(false, "", s"${actual.description} is not a Result error", actual),
        _         => result(false, "", s"${actual.description} is not a Result error", actual),
        (msg, ex) => f.andThen(mr => result(mr.toResult, actual)).applyOrElse(
          (msg, ex),
          { t: (String, Throwable) => result(false, "", s"($msg, $ex) is not the expected error", actual) }
        )
      )
    }
  }
}
