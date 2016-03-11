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

/** extend RelMonad[M, N] instances to RelMonad[M, ReaderT[N, Rd, _]] */
class ReaderTR[M[_] : Monad, N[_] : Monad, Rd](MrelN: RelMonad[M, N])
    extends RelMonad[M, ({ type readerN[A]=ReaderT[N, Rd, A] })#readerN] {
  val N = Monad[N]

  type ReaderN[A] = ReaderT[N, Rd, A]
  val readerN = Monad[ReaderN]
  def mkReaderN[A](f: Rd => N[A]) = new ReaderT[N, Rd, A](f)

  def rPoint[A](v: => M[A]): ReaderN[A] = new ReaderT(_ => MrelN.rPoint(v))
  def rBind[A, B](rNA: ReaderN[A])(f: M[A] => ReaderN[B]): ReaderN[B] =
    readerN.join(mkReaderN[ReaderN[B]](rd =>
                   MrelN.rBind(rNA(rd))(ma => N.point(f(ma)))
                 ))
  def relMonad: RelMonad[M, ReaderN] = this
}

