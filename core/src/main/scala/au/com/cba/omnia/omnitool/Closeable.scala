package au.com.cba.omnia.omnitool

import java.io.{Closeable => JCloseable}

case class Closeable[T](close: T => Any)

case class CloseableSyntax[T](v: T, closeable: Closeable[T]) {
  /** Close the resource */
  def close(): Unit = closeable.close(v)

  /** Perform ```thunk``` using the resource and then close it */
  def doAndRelease[A](thunk: T => A): A = Closeable.doAndRelease(v, thunk, closeable.close)
}

object Closeable extends CloseableOps {
  implicit def JCloseableToCloseable[T <: JCloseable] = Closeable[T](_.close)

  /** Perform ```thunk``` on ```v``` and then close it using the specified ```close``` */
  def doAndRelease[T, A](v: T, thunk: T => A, close: T => Any): A = try thunk(v) finally close(v)
}

trait CloseableOps {
  implicit def CloseableToCloseableSyntax[T : Closeable](value: T) = CloseableSyntax(value, implicitly[Closeable[T]])
}
