package com.phasmid.laScala.fp

import java.io.Closeable

import com.phasmid.laScala.parser.rpn.RPN
import org.scalatest.{FlatSpec, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success}

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class SmartLoggerSpec extends FlatSpec with Matchers {
  "SmartLogger" should "work" in {
    val logger = SmartLoggerSlf4JInfo(LoggerFactory.getLogger(getClass))
    logger("")("99".toInt) shouldBe 99
  }
  it should "leave the proper trail" in {
    var elephantMemory = ""
    def elephantLogger(s: String) = {
      elephantMemory = elephantMemory + s + "\n"
    }
    val logger = SmartLoggerBasic(elephantLogger)
    logger("99"){
      "99".toInt
    } shouldBe 99
    elephantMemory shouldBe "Starting 99\nFinished 99 with result: 99\n"
  }
  it should "work in context" in {
    val logger = LoggerFactory.getLogger(getClass)
    import SmartLoggerSlf4JInfo._
    (for (i <- List("1", "2", "3")) yield logger("string conversion")(i.toInt)) shouldBe List(1,2,3)
  }
  it should "log and re-throw exception" in {
    val logger = LoggerFactory.getLogger(getClass)
    import SmartLoggerSlf4JInfo._
    an [SmartLoggerException] should be thrownBy
    (for (i <- List("1", "a", "3")) yield logger("string conversion")(i.toInt))
  }
//  it should "work in context with debug" in {
//    val logger = LoggerFactory.getLogger(getClass)
//    import SmartLoggerSlf4JDebug._
//    (for (i <- List("1", "2", "3")) yield logger.wrapDebug("string conversion")(i.toInt)) shouldBe List(1,2,3)
//  }
}
