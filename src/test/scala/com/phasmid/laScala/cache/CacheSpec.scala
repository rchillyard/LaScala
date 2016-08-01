package com.phasmid.laScala.cache

import akka.event.LoggingAdapter
import com.phasmid.laScala.FP
import org.scalatest._
import org.scalatest.concurrent._

import scala.language.implicitConversions
import scala.util._

/**
  * Created by scalaprof on 3/28/16.
  */

class CacheSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  class MockLoggingAdapter extends LoggingAdapter {
    var debugCount = 0
    var infoCount = 0
    var warningCount = 0
    var errorCount = 0

    protected def notifyInfo(message: String): Unit = {
      println("info: " + message)
      infoCount = infoCount + 1
    }

    override def isErrorEnabled: Boolean = false

    override def isInfoEnabled: Boolean = true

    override def isDebugEnabled: Boolean = true

    override protected def notifyError(message: String): Unit = {
      System.err.println("error: " + message)
      errorCount = errorCount + 1
    }

    override protected def notifyError(cause: Throwable, message: String): Unit = {
      System.err.println("error: " + message + ": " + cause)
      errorCount = errorCount + 1
    }

    override def isWarningEnabled: Boolean = true

    override protected def notifyWarning(message: String): Unit = {
      System.err.println("warning: " + message)
      warningCount = warningCount + 1
    }

    override protected def notifyDebug(message: String): Unit = {
      println("debug: " + message)
      debugCount = debugCount + 1
    }
  }

  "BasicFulfillingCache" should "succeed" in {
    val mockLoggingAdapter = new MockLoggingAdapter
    mockLoggingAdapter.debugCount shouldBe 0
    mockLoggingAdapter.warningCount shouldBe 0
    implicit def carper(s: String): Unit = mockLoggingAdapter.warning(s)
    def evaluate(k: String): Option[Int] = {
      mockLoggingAdapter.debug(s"evaluating $k")
      Try(k.toInt).toOption
    }
    val cache = BasicFulfillingCache[String, Int](evaluate)
    mockLoggingAdapter.warningCount shouldBe 0
    cache.get("1") should matchPattern { case Some(1) => }
    mockLoggingAdapter.warningCount shouldBe 0
    mockLoggingAdapter.debugCount shouldBe 1
    cache("1") should matchPattern { case 1 => }
    mockLoggingAdapter.warningCount shouldBe 0
    mockLoggingAdapter.debugCount shouldBe 1
    cache.get("2") should matchPattern { case Some(2) => }
    mockLoggingAdapter.warningCount shouldBe 0
    mockLoggingAdapter.debugCount shouldBe 2
    cache.get("A") should matchPattern { case None => }
    mockLoggingAdapter.warningCount shouldBe 1
    cache.get("A") should matchPattern { case None => }
    mockLoggingAdapter.warningCount shouldBe 2
  }
  "NonExpiringCache" should "succeed" in {
    val mockLoggingAdapter = new MockLoggingAdapter
    mockLoggingAdapter.debugCount shouldBe 0
    mockLoggingAdapter.warningCount shouldBe 0
    implicit def carper(s: String): Unit = mockLoggingAdapter.warning(s)
    def evaluate(k: String): Try[Int] = {
      mockLoggingAdapter.debug(s"evaluating $k")
      Try(k.toInt)
    }
    val cache = NonExpiringCache[String, Int](evaluate)
    mockLoggingAdapter.warningCount shouldBe 0
    cache.get("1") should matchPattern { case Success(1) => }
    mockLoggingAdapter.warningCount shouldBe 0
    mockLoggingAdapter.debugCount shouldBe 1
    cache("1") should matchPattern { case 1 => }
    mockLoggingAdapter.warningCount shouldBe 0
    mockLoggingAdapter.debugCount shouldBe 1
    cache.get("2") should matchPattern { case Success(2) => }
    mockLoggingAdapter.warningCount shouldBe 0
    mockLoggingAdapter.debugCount shouldBe 2
    cache.get("A") should matchPattern { case Failure(_) => }
    mockLoggingAdapter.warningCount shouldBe 1
    cache.get("A") should matchPattern { case Failure(_) => }
    mockLoggingAdapter.warningCount shouldBe 2
  }
  "asMap" should "yield appropriate Map" in {
    val values = Map("x" -> 1, "y" -> 2)
    import Cache._
    val cache = NonExpiringCache[String, Int] { k => FP.optionToTry(values.get(k)) }
    cache("x")
    val map: Map[String, Int] = cache.asMap
    map shouldBe Map("x" -> 1)
    cache("y")
    cache.asMap shouldBe Map("x" -> 1, "y" -> 2)
  }
}