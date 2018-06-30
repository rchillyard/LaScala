package com.phasmid.laScala.fp

import com.phasmid.laScala.MockLogger
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

/**
  * Created by scalaprof on 9/7/16.
  */
class LogSpec extends FlatSpec with Matchers {

  behavior of "Log"

  it should "log Int" in {
    import Log.logger
    val log = Log()(1)
    log.log("test") shouldBe 1
  }
  it should "log using MockLogger" in {
    val sb = new StringBuilder
    implicit val logger: MockLogger = MockLogger("mockLogger", "DEBUG", sb)
    val log = Log()(1)
    log.log("test") shouldBe 1
    sb.toString() shouldBe "mockLogger: DEBUG: test: 1\n"
  }
  it should "log with logger function" in {
    val sb = new StringBuilder
    implicit val logger: MockLogger = MockLogger("mockLogger", "DEBUG", sb)
    val f = Log[Int]() _
    f(1).log("test") shouldBe 1
    sb.toString() shouldBe "mockLogger: DEBUG: test: 1\n"
  }
  it should "log Int using {}" in {
    val sb = new StringBuilder
    implicit val logger: MockLogger = MockLogger("mockLogger", "DEBUG", sb)
    val log = Log()(1)
    log.log("x: {}.") shouldBe 1
    sb.toString() shouldBe "mockLogger: DEBUG: x: 1.\n"
  }

  import Log._

  behavior of "Log (implicit)"

  it should "log Int" in {
    1.log("test") shouldBe 1
  }
  it should "log Int with {}" in {
    val x = 1
    x.log(s"value {} should be $x") shouldBe 1
  }
  it should "log Double" in {
    1.0.log("test") shouldBe 1
  }
  it should "log Long" in {
    1L.log("test") shouldBe 1
  }
  it should "log BigInt" in {
    BigInt(1).log("test") shouldBe 1
  }
  it should "log String" in {
    "1".log("test") shouldBe "1"
  }
  it should "log Char" in {
    'A'.log("test") shouldBe 'A'
  }
  it should "log List" in {
    List(1).log("test") shouldBe List(1)
  }
  it should "log as usual and not use MockLogger" in {
    val sb = new StringBuilder
    implicit val logger: MockLogger = MockLogger("mockLogger", "DEBUG", sb)
    1.log("test") shouldBe 1
    sb.toString shouldBe ""
  }
  it should "log as in an application" in {
    val x = for (i <- 1 to 3) yield i.log("i")*i
    x shouldBe Seq(1, 4, 9)
  }

}
