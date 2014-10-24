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

import scala.collection.immutable.Stream

import org.apache.log4j.{RollingFileAppender, Level, Logger, PatternLayout}

/**
  * Convienience methods on log4j [[org.apache.log4j.Logger]].
  *
  * The user can call [[RichLogger.child]] to create a child of another
  * logger with different configuration, or they can call [[RichLogger.set]]
  * to change the configuration of an existing logger.
  *
  * The user can pass in any number of functions which change a logger's
  * configuration. They can use [[RichLogger.logToFile]] and [[RichLogger.logInfo]],
  * or create their own configuration functions.
  *
  * Example usage, creating a child logger that outputs info messages
  * to a file, but does not ouput to any of the parent logger appenders:
  *
  * {{{
  * import org.apache.log4j.Logger
  * import au.com.cba.omnia.omnitool.log.RichLogger._
  *
  * val mylogger = Logger.getLogger("mylogger")
  *
  * mylogger.child("stats",
  *   _.setAdditivity(false), // a config function that just calls a Logger method
  *   logToFile("myfile"),    // the logToFile config function below
  *   logInfo                 // the logInfo config function below
  * )
  * }}}
  */
class RichLogger(logger: Logger) {
  /** Create a child of the logger with additional configuration */
  def child(name: String, options: RichLogger.LoggerConfig*): Logger = {
    val childLogger = Logger.getLogger(logger.getName + "." + name)
    options.foreach(_(childLogger))
    childLogger
  }

  /** Set configuration options on a logger */
  def set(options: RichLogger.LoggerConfig*) {
    options.foreach(_(logger))
  }
}

/**
  * Implicit conversion for [[RichLogger]],
  * and configuration functions to be used with [[RichLogger.set]] or [[RichLogger.child]].
  */
object RichLogger {

  implicit def loggerMethods(logger: Logger): RichLogger =
    new RichLogger(logger)

  /**
    * Type of configuration functions for Logger,
    * passed to [[RichLogger.set]] or [[RichLogger.child]].
    *
    */
  type LoggerConfig = Logger => Unit

  /**
    * Adds a rolling file appender to the logger.
    *
    * @param file: The local file to log to.
    * @param pattern: The [[org.apache.log4j.PatternLayout]] pattern for log messages. Defaults to just displaying the log message and no other info.
    * @param maxFileSize: The size of the log file before we roll it over and start a new one. Defaults to 60 MB.
    */
  def logToFile(
    file: String,
    pattern: String = PatternLayout.DEFAULT_CONVERSION_PATTERN,
    maxFileSize: String = "60MB"
  ): LoggerConfig =
    (logger: Logger) => {
      val uniqueName = {
        val baseName = logger.getName + ".fileAppender"
        val possibleNames = logger.getName #:: Stream.from(2).map(baseName + _)
        possibleNames.filter(logger.getAppender(_) == null).head
      }

      val fa = new RollingFileAppender
      fa.setName(uniqueName)
      fa.setFile(file)
      fa.setLayout(new PatternLayout(pattern))
      fa.setAppend(true)
      fa.setMaxFileSize(maxFileSize)
      fa.setMaxBackupIndex(Int.MaxValue)
      fa.activateOptions
      logger.addAppender(fa)
    }

  /** Sets the logger level to info */
  val logInfo: LoggerConfig = (logger: Logger) => logger.setLevel(Level.INFO)
}
