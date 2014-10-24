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

package au.com.cba.omnia.omnitool.log

import java.io.File

import scala.io.Source

import org.apache.log4j.{Level, Logger}

import com.cba.omnia.test.Spec

import au.com.cba.omnia.omnitool.log.RichLogger._

object LoggerMethodsSpec extends Spec { def is = s2"""
LoggerMethodsSpec
=================

 child should:
   create a child of given logger     $isChild
   apply config changes to child      $childChanged
   not apply config changes to parent $parentUnchanged

 set should:
   change the logger $loggerChanged

 logToFile should:
   add a file logger $loggingToFile

"""

  def isChild = {
    val parent = Logger.getLogger("isChild")
    val child = parent.child("child")
    parent eq child.getParent must beTrue
  }

  def childChanged = {
    val parent = Logger.getLogger("childChanged")
    parent.setLevel(Level.ERROR)
    val child = parent.child("child", _.setLevel(Level.TRACE))
    child.isEnabledFor(Level.TRACE) must beTrue
  }

  def parentUnchanged = {
    val parent = Logger.getLogger("parentUnchanged")
    parent.setLevel(Level.ERROR)
    val child = parent.child("child", _.setLevel(Level.TRACE))
    parent.isEnabledFor(Level.TRACE) must beFalse
  }

  def loggerChanged = {
    val logger = Logger.getLogger("loggerChanged")
    logger.setLevel(Level.ERROR)
    logger.isEnabledFor(Level.TRACE) must beFalse
    logger.set(_.setLevel(Level.TRACE))
    logger.isEnabledFor(Level.TRACE) must beTrue
  }

  def loggingToFile = {
    val file = File.createTempFile("loggerMethodsSpec", ".txt")
    try {
      val logger = Logger.getLogger("loggingToFile").child("child", logInfo, logToFile(file.toString))
      logger.info("foo")
      logger.info("bar")
      Source.fromFile(file).getLines.toList must_== List("foo", "bar")
    }
    finally {
      file.delete
    }
  }
}
