package com.phasmid.laScala.fp

import org.slf4j.{Logger, LoggerFactory}

import scala.language.implicitConversions

/**
  * Created by scalaprof on 8/17/16.
  */
trait SmartLogger {
  def logFunc: String => Unit

  def startFormatter(name: String): String = s"Starting $name"

  def endFormatter(name: String, x: => String): String = s"Finished $name with result: $x"

  def exceptionCreator(name: String, ex: Throwable): Throwable = new SmartLoggerException(s"Exception thrown in $name", ex)

  def errorFunc: (String, Throwable) => Unit

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

case class SmartLoggerBasic(logFunc: String => Unit, override val errorFunc: (String,Throwable) => Unit) extends SmartLogger

object SmartLoggerBasic {
  def apply(): SmartLogger = apply(println, {(s,x) => System.err.println(s"Exception $s: ${x.getLocalizedMessage}")})
}

case class SmartLoggerSlf4JInfo(logger: Logger) extends SmartLogger {
  def logFunc = logger.info
  def errorFunc = { (s, x) => logger.warn(s"Exception thrown for $s: ${x.getLocalizedMessage}") }
}

case class SmartLoggerSlf4JDebug(logger: Logger) extends SmartLogger {
  def logFunc = logger.debug
  def errorFunc = { (s, x) => logger.warn(s"Exception thrown for $s: ${x.getLocalizedMessage}") }
}

object SmartLoggerSlf4JInfo {
  def apply(clazz: Class[_]): SmartLogger = apply(LoggerFactory.getLogger(clazz))

  implicit def logAround(logger: Logger): SmartLogger = SmartLoggerSlf4JInfo(logger)
}

object SmartLoggerSlf4JDebug {
  def apply(clazz: Class[_]): SmartLogger = apply(LoggerFactory.getLogger(clazz))

  implicit def logAround(logger: Logger): SmartLogger = SmartLoggerSlf4JDebug(logger)
}

class SmartLoggerException(s: String, ex: Throwable) extends Exception(s, ex)