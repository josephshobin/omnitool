package au.com.cba.omnia.omnitool

import scalaz._, Scalaz._, Free.Trampoline

object TrampolineResult {

  type TrampoResult[A] = Trampoline[Result[A]]
  val Trampoline = implicitly[Monad[Trampoline]]

  implicit def ResultRel: RelMonad[Result, TrampoResult] = new RelMonad[Result, TrampoResult] {
    def rPoint[A](v: => Result[A]): Trampoline[Result[A]] = {
      Trampoline.point(v)
    }
    def rBind[A, B](m: Trampoline[Result[A]])(f: Result[A] => Trampoline[Result[B]]) = {
      Trampoline.bind(m)(f)
    }
  }
}

