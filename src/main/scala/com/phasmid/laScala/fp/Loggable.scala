package com.phasmid.laScala.fp

import org.slf4j.Logger

/**
  * Created by scalaprof on 8/17/16.
  */
trait Loggable {
  def logFunc: String=>Unit
  def invoke[V](f: =>V, message: String = "<unnamed>"): V = {
    logFunc(s"starting $message")
    val x = f
    logFunc(s"finished $message")
    x
    }
}

case class LoggableBasic(logFunc: String=>Unit) extends Loggable

case class LoggableSlf4jInfo(logger: Logger) extends Loggable {
  def logFunc = logger.info _
}

