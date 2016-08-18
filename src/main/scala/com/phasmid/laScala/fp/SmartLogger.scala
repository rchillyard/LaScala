package com.phasmid.laScala.fp

import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by scalaprof on 8/17/16.
  */
trait SmartLogger {
  def logFunc: String => Unit

  def startFormatter(name: String): String = s"Starting $name"

  def endFormatter(name: String, x: => String): String = s"Finished $name with result: $x"

  def exceptionCreator(name: String, ex: Throwable): Throwable = SmartLoggerException(s"Exception thrown in $name", ex)

  def errorFunc: (String, Throwable) => Unit = { (s, x) => logFunc(s"$s: ${x.getLocalizedMessage}") }

  def apply[V](name: String)(f: => V): V = {
    logFunc(startFormatter(name))
    try {
      val x = f
      logFunc(endFormatter(name, x.toString))
      x
    }
    catch {
      case ex: Throwable => errorFunc(name, ex); throw exceptionCreator(name, ex)
    }
  }
}

case class SmartLoggerBasic(logFunc: String => Unit) extends SmartLogger

case class SmartLoggerSlf4JInfo(logger: Logger) extends SmartLogger {
  def logFunc = logger.info _
}

case class SmartLoggerSlf4JDebug(logger: Logger) extends SmartLogger {
  def logFunc = logger.debug _
}

object SmartLoggerSlf4JInfo {
  def apply(clazz: Class[_]): SmartLogger = apply(LoggerFactory.getLogger(clazz))

  implicit def logAround(logger: Logger): SmartLogger = SmartLoggerSlf4JInfo(logger)
}

object SmartLoggerSlf4JDebug {
  def apply(clazz: Class[_]): SmartLogger = apply(LoggerFactory.getLogger(clazz))

  implicit def logAround(logger: Logger): SmartLogger = SmartLoggerSlf4JDebug(logger)
}

case class SmartLoggerException(s: String, ex: Throwable) extends Exception(s, ex)