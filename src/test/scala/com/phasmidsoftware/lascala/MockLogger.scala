/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala

import org.slf4j.Marker
import org.slf4j.event.Level
import org.slf4j.helpers.{LegacyAbstractLogger, MessageFormatter}

/**
  * MockLogger for unit testing log messages.
  *
  * Every message that is enabled for the given level is appended to sb as "name: level: message".
  * LegacyAbstractLogger implements all the methods of Logger in terms of handleNormalizedLoggingCall.
  *
  * @param loggerName the name of this logger
  * @param level      the level of this logger
  * @param sb         a StringBuilder to which we append messages
  */
case class MockLogger(loggerName: String, level: String, sb: StringBuilder) extends LegacyAbstractLogger {
  name = loggerName

  def isTraceEnabled: Boolean = level == "TRACE"

  def isDebugEnabled: Boolean = level == "DEBUG" || isTraceEnabled

  def isInfoEnabled: Boolean = level == "INFO" || isDebugEnabled

  def isWarnEnabled: Boolean = level == "WARN" || isInfoEnabled

  def isErrorEnabled: Boolean = level == "ERROR" || isWarnEnabled

  protected def getFullyQualifiedCallerName: String = null

  protected def handleNormalizedLoggingCall(l: Level, marker: Marker, messagePattern: String, arguments: Array[AnyRef], throwable: Throwable): Unit =
    sb.append(s"$loggerName: $level: ${MessageFormatter.basicArrayFormat(messagePattern, arguments)}\n")
}
