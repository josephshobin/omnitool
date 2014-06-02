package au.com.cba.omnia.omnitool

import Closeable._

class CloseableSpec extends OmnitoolTest { def is = s2"""

Error handling
==============

Closeable:
  Can be used to perform an operation on a Closeable and then close it $doAndRelease
  Can be used to perform an operation on a resource and then close it if a close operation has been supplied $doAndReleaseManual

"""

  class TestClose(var count: Int, var closed: Boolean = false) {
    def inc = {
      count += 1
      count
    }

    def close() = if (!closed) closed = true else throw new Exception("called close twiece")

  }

  object TestClose {
    implicit def TestCloseToCloseable = Closeable[TestClose]((x: TestClose) => x.close)
  }

  def doAndRelease = {
    val x = new TestClose(0)
    val count = x.doAndRelease(_.inc)
    count === 1
    x.count === 1
    x.closed must beTrue
  }

  def doAndReleaseManual = {
    val x = new TestClose(0)
    val count = Closeable.doAndRelease[TestClose, Int](x, _.inc, _.close)
    count === 1
    x.count === 1
    x.closed must beTrue
  }
}
