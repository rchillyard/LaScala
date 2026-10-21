/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala

import org.slf4j.{Logger, Marker}

/**
  * MockLogger for unit testing log messages
  *
  * TODO implement all of the unimplemented stuff
  *
  * @param name  the name of this logger
  * @param level the level of this logger
  * @param sb    a StringBuilder to which we append messages
  */
//noinspection TypeAnnotation,NotImplementedCode
case class MockLogger(name: String, level: String, sb: StringBuilder) extends Logger {
  def getName = name

  def debug(msg: String) = doLog(msg)

  private def doLog(msg: String) = sb.append(s"$name: $level: $msg\n")

  def debug(format: String, arg: scala.Any): Nothing = ???

  def debug(format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def debug(format: String, arguments: AnyRef*) = ???

  def debug(msg: String, t: Throwable): Nothing = ???

  def debug(marker: Marker, msg: String): Nothing = ???

  def debug(marker: Marker, format: String, arg: scala.Any): Nothing = ???

  def debug(marker: Marker, format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def debug(marker: Marker, format: String, arguments: AnyRef*) = ???

  def debug(marker: Marker, msg: String, t: Throwable): Nothing = ???

  def isWarnEnabled = level == "WARN" || isInfoEnabled

  def isWarnEnabled(marker: Marker): Nothing = ???

  def error(msg: String) = doLog(msg)

  def error(format: String, arg: scala.Any): Nothing = ???

  def error(format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def error(format: String, arguments: AnyRef*) = ???

  def error(msg: String, t: Throwable): Nothing = ???

  def error(marker: Marker, msg: String): Nothing = ???

  def error(marker: Marker, format: String, arg: scala.Any): Nothing = ???

  def error(marker: Marker, format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def error(marker: Marker, format: String, arguments: AnyRef*) = ???

  def error(marker: Marker, msg: String, t: Throwable): Nothing = ???

  def warn(msg: String) = doLog(msg)

  def warn(format: String, arg: scala.Any): Nothing = ???

  def warn(format: String, arguments: AnyRef*) = ???

  def warn(format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def warn(msg: String, t: Throwable): Nothing = ???

  def warn(marker: Marker, msg: String): Nothing = ???

  def warn(marker: Marker, format: String, arg: scala.Any): Nothing = ???

  def warn(marker: Marker, format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def warn(marker: Marker, format: String, arguments: AnyRef*) = ???

  def warn(marker: Marker, msg: String, t: Throwable): Nothing = ???

  def trace(msg: String) = doLog(msg)

  def trace(format: String, arg: scala.Any): Nothing = ???

  def trace(format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def trace(format: String, arguments: AnyRef*) = ???

  def trace(msg: String, t: Throwable): Nothing = ???

  def trace(marker: Marker, msg: String): Nothing = ???

  def trace(marker: Marker, format: String, arg: scala.Any): Nothing = ???

  def trace(marker: Marker, format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def trace(marker: Marker, format: String, argArray: AnyRef*) = ???

  def trace(marker: Marker, msg: String, t: Throwable): Nothing = ???

  def isInfoEnabled = level == "INFO" || isDebugEnabled

  def isInfoEnabled(marker: Marker): Nothing = ???

  def isErrorEnabled = level == "ERROR" || isWarnEnabled

  def isErrorEnabled(marker: Marker): Nothing = ???

  def isTraceEnabled = level == "TRACE"

  def isTraceEnabled(marker: Marker): Nothing = ???

  def isDebugEnabled = level == "DEBUG" || isTraceEnabled

  def isDebugEnabled(marker: Marker): Nothing = ???

  def info(msg: String) = doLog(msg)

  def info(format: String, arg: scala.Any): Nothing = ???

  def info(format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def info(format: String, arguments: AnyRef*) = ???

  def info(msg: String, t: Throwable): Nothing = ???

  def info(marker: Marker, msg: String): Nothing = ???

  def info(marker: Marker, format: String, arg: scala.Any): Nothing = ???

  def info(marker: Marker, format: String, arg1: scala.Any, arg2: scala.Any): Nothing = ???

  def info(marker: Marker, format: String, arguments: AnyRef*) = ???

  def info(marker: Marker, msg: String, t: Throwable): Nothing = ???
}
