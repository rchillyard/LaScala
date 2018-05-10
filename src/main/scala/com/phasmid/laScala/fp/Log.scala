package com.phasmid.laScala.fp

import org.slf4j.{Logger, LoggerFactory}

import scala.util._

/**
  * Case class to support functional logging.
  *
  * NOTE: the non-strict String parameter (w) is not evaluated if debug is not enabled for the logger
  *
  * NOTE: the empty parentheses are because a case class cannot have a non-strict (i.e. call-by-name) parameter in its first parameter set
  *
  * @param x the lazy value of X
  * @tparam X the type of x and the return type of method log
  */
case class Log[X]()(x: => X)(implicit logger: Logger) {

  def log(w: => String): X =
    Try(x) match {
      case Success(z) => if (logger.isDebugEnabled()) logger.debug(s"$w: $z"); z
      case Failure(t) => logger.warn(s"Log: Exception thrown for $w", t); throw t
    }
}

object Log {

  import scala.language.implicitConversions

  implicit val logger: Logger = LoggerFactory.getLogger(classOf[Log[_]])

  implicit def logInt(x: => Int): Log[Int] = Log()(x)

  implicit def logDouble(x: => Double): Log[Double] = Log()(x)

  implicit def logString(x: => String): Log[String] = Log()(x)

  implicit def logLong(x: => Long): Log[Long] = Log()(x)

  implicit def logBigInt(x: => BigInt): Log[BigInt] = Log()(x)

  implicit def logChar(x: => Char): Log[Char] = Log()(x)

  implicit def logTraversable(x: => Traversable[_]): Log[Traversable[_]] = Log()(x)
}

