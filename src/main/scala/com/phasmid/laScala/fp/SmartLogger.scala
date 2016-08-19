package com.phasmid.laScala.fp

import org.slf4j.{Logger, LoggerFactory}

import scala.language.implicitConversions

/**
  * FP implementation of a logger.
  *
  * Created by scalaprof on 8/17/16.
  */
trait SmartLogger {
  /**
    * The apply function of the SmartLogger. This method can be applied to any expression such that the start of the
    * expression will be logged, as well as the end of the epxression.
    *
    * Note that the type V is defined only for this method, not for the SmartLogger
    * trait. That way we can use the same SmartLogger for any expression.
    *
    * @param name the name (identifier) of the expression that we are logging
    * @param f the function which implements the expression
    * @tparam V the type of the expression's result
    * @return the result of the expression
    */
  def apply[V](name: String)(f: => V): V = {
    val startMsg = startFormatter(name)
    if (startMsg!=null) logFunc(startMsg)
    try {
      val x = f
      val endMsg = endFormatter(name, x.toString)
      if (endMsg!=null) logFunc(endMsg)
      x
    }
    catch {
      case ex: Throwable => errorFunc(name, ex); throw exceptionCreator(name, ex)
    }
  }

  /**
    * The function to be used for logging the start and end. Normally this will be something like the debug method of
    * a standard logger.
    *
    * @return nothing
    */
  def logFunc: String => Unit

  /**
    * The function to be used for logging any thrown exceptions. Normally this will be something like the warn method of
    * a standard logger.
    *
    * @return nothing
    */
  def errorFunc: (String, Throwable) => Unit

  /**
    * The formatter for the start message.
    *
    * @param name the name/identifier of the expression
    * @return a String to be output. If the string is null, then no start message will be logged
    */
  def startFormatter(name: String): String = s"Starting $name"

  /**
    * The formatter for the end message.
    *
    * @param name the name/identifier of the expression
    * @param x the String representing the expression's result (call-by-name so that it isn't evaluated if we don't need it)
    * @return a String to be output. If the string is null, then no end message will be logged
    */
  def endFormatter(name: String, x: => String): String = s"Finished $name with result: $x"

  /**
    * A method to create an exception to be returned to the caller in the event of an exception being thrown by the expression
    * @param name the name/identifier of the expression
    * @param ex the exception that was actually thrown
    * @return an exception which will be thrown by the apply method. Normally this will wrap the thrown exception.
    */
  def exceptionCreator(name: String, ex: Throwable): Throwable = new SmartLoggerException(s"Exception thrown in $name", ex)
}

/**
  * A basic logger.
  *
  * @param logFunc the logger function
  * @param errorFunc the error function
  */
case class SmartLoggerBasic(logFunc: String => Unit, override val errorFunc: (String,Throwable) => Unit) extends SmartLogger

object SmartLoggerBasic {
  /**
    * Method to create a SmartLoggerBasic based on standard output and error output.
    * @return a SmartLoggerBasic
    */
  def apply(): SmartLogger = apply(println, {(s,x) => System.err.println(s"Exception $s: ${x.getLocalizedMessage}")})
}

/**
  * A logger which will log as info on an Slf4J logger
  *
  * @param logger an Slf4J logger
  */
case class SmartLoggerSlf4JInfo(logger: Logger) extends SmartLogger {
  def logFunc = logger.info
  def errorFunc = { (s, x) => logger.warn(s"Exception thrown for $s: ${x.getLocalizedMessage}") }
}

/**
  * A logger which will log as debug on an Slf4J logger
  *
  * @param logger an Slf4J logger
  */
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