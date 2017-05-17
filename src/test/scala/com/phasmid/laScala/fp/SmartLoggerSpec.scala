package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class SmartLoggerSpec extends FlatSpec with Matchers {

  class ElephantLogger {
    private var memory = ""

    def log(s: String): Unit = {
      memory = memory + s + "\n"
    }

    def get: String = memory
  }

  "SmartLogger" should "work" in {
    val logger = SmartLoggerSlf4JInfo(LoggerFactory.getLogger(getClass))
    logger("")("99".toInt) shouldBe 99
  }
  it should "leave the proper trail" in {
    val regularLogger = new ElephantLogger
    val errorLogger = new ElephantLogger
    val logger = SmartLoggerBasic(regularLogger.log, { (s, x) => errorLogger.log(s"Error in $s: ${x.getLocalizedMessage}") })
    logger("99") {
      "99".toInt
    } shouldBe 99
    regularLogger.get shouldBe "Starting 99\nFinished 99 with result: 99\n"
  }
  it should "leave the proper trail after error" in {
    val regularLogger = new ElephantLogger
    val errorLogger = new ElephantLogger
    val logger = SmartLoggerBasic(regularLogger.log, { (s, x) => errorLogger.log(s"Error in $s: ${x.getLocalizedMessage}") })
    an[SmartLoggerException] should be thrownBy
      logger("xToInt")("x".toInt)
    errorLogger.get shouldBe "Error in xToInt: For input string: \"x\"\n"
  }
  it should "work in context" in {
    val logger = LoggerFactory.getLogger(getClass)
    import SmartLoggerSlf4JInfo._
    (for (i <- List("1", "2", "3")) yield logger("string conversion")(i.toInt)) shouldBe List(1, 2, 3)
  }
  it should "log and re-throw exception" in {
    val logger = LoggerFactory.getLogger(getClass)
    import SmartLoggerSlf4JInfo._
    an[SmartLoggerException] should be thrownBy
      (for (i <- List("1", "a", "3")) yield logger("string conversion")(i.toInt))
  }
  it should "work in context with debug" in {
    val logger = LoggerFactory.getLogger(getClass)
    import SmartLoggerSlf4JDebug._
    // NOTE that nothing will show in the logs unless you redefine the level to be DEBUG
    (for (i <- List("1", "2", "3")) yield logger("string conversion")(i.toInt)) shouldBe List(1, 2, 3)
  }
  it should "work with milestones" in {
    val logger = LoggerFactory.getLogger(getClass)
    import SmartLoggerSlf4JInfo._
    (for (i <- List("1", "2", "3")) yield logger("string conversion") {
      val x = i.toInt
      logger.milestone(s"x=$x")()
      val y = x.toDouble
      logger.milestone(s"have y")(y)
      val z = x.toFloat
      logger.milestone(s"have z")(y, z)
      z
    }) shouldBe List(1, 2, 3)
  }
  it should "work with milestones with elephant memory" in {
    val regularLogger = new ElephantLogger
    val errorLogger = new ElephantLogger
    val logger = SmartLoggerBasic(regularLogger.log, { (s, x) => errorLogger.log(s"Error in $s: ${x.getLocalizedMessage}") })
    for (i <- List("1", "2", "3")) yield logger("string conversion") {
      val x = i.toInt
      logger.milestone(s"x=$x")()
      val y = x.toDouble
      logger.milestone(s"have y")(y)
      val z = x.toFloat
      logger.milestone()(y, z)
      z
    }
    regularLogger.get shouldBe """Starting string conversion
Milestone x=1
Milestone have y: 1.0
Milestone 1: 1.0, 1.0
Finished string conversion with result: 1.0
Starting string conversion
Milestone x=2
Milestone have y: 2.0
Milestone 2: 2.0, 2.0
Finished string conversion with result: 2.0
Starting string conversion
Milestone x=3
Milestone have y: 3.0
Milestone 3: 3.0, 3.0
Finished string conversion with result: 3.0
"""
  }
}
