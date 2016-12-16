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

import java.io.{Closeable => JCloseable}

case class Closeable[T](close: T => Unit)

case class CloseableSyntax[T](v: T, closeable: Closeable[T]) {
  /** Close the resource */
  def close(): Unit = closeable.close(v)

  /** Perform ```thunk``` using the resource and then close it */
  def doAndRelease[A](thunk: T => A): A = Closeable.doAndRelease(v, thunk, closeable.close)
}

object Closeable extends CloseableOps {
  implicit def JCloseableToCloseable[T <: JCloseable] = Closeable[T](_.close)

  /** Perform ```thunk``` on ```v``` and then close it using the specified ```close``` */
  def doAndRelease[T, A](v: T, thunk: T => A, close: T => Unit): A = try thunk(v) finally close(v)
}

trait CloseableOps {
  implicit def CloseableToCloseableSyntax[T : Closeable](value: T) = CloseableSyntax(value, implicitly[Closeable[T]])
}
