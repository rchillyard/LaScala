package com.phasmid.laScala.fp

import com.phasmid.laScala.MockLogger
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.Logger

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

/**
  * Created by scalaprof on 8/5/16.
  */
class SpySpec extends FlatSpec with Matchers {

  behavior of "Spy.spy"
  it should "work with implicit (logger) (with default logger) spy func" in {
    import Spy._
    Spy.spying = true
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1, 2)
    // you should see log messages written to console (assuming your logging level, i.e. logback-test.xml, is set to DEBUG)
  }
  it should "work with implicit (logger) spy func (with logger for this class) on the map2 function" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    Spy.spying = true
    val x = Some(1)
    FP.map2(x, x)(_ + _)
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1, 2)
    // you should see log messages written to console (assuming your logging level, i.e. logback-test.xml, is set to DEBUG)
  }
  it should "work with implicit (logger) spy func but with custom logger" in {
    Spy.spying = true
    implicit val logger: Logger = org.slf4j.LoggerFactory.getLogger("myLogger")
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1, 2)
    // you should see log messages written to console (assuming your logging level, i.e. logback-test.xml, is set to DEBUG)
  }
  it should "work with implicit (logger) spy func but with custom mock logger" in {
    Spy.spying = true
    val sb = new StringBuilder
    implicit val logger: MockLogger = MockLogger("myLogger", "DEBUG", sb)
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1, 2)
    sb.toString() shouldBe "myLogger: DEBUG: spy: i: 1\nmyLogger: DEBUG: spy: i: 2\n"
  }
  it should "work with explicit spy func" in {
    import Spy._
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"explicit spy: $s\n")

    val is = for (i <- 1 to 2) yield Spy.spy("i", i)
    is shouldBe List(1, 2)
    spyMessage shouldBe "explicit spy: i: 1\nexplicit spy: i: 2\n"
  }
  it should "work with explicit spy func when exception is thrown" in {
    import Spy._
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"explicit spy: $s\n")

    try Spy.spy("division by zero", 1 / 0)
    catch { case _: Exception => }
    spyMessage shouldBe "explicit spy: division by zero: <<Exception thrown: / by zero>>\n"
  }
  it should "work with explicit custom println spy func" in {
    import Spy._
    implicit def spyFunc(s: String): Spy = Spy(println(s))

    Spy.spying = true
    val is = for (i <- 1 to 2) yield Spy.spy("mySpy: i", i)
    is shouldBe List(1, 2)
    // you should see messages written to console with "mySPy" prefix
  }
  it should "work with explicit provided println spy func" in {
    import Spy._
    implicit val spyFunc: (String) => Spy = Spy.getPrintlnSpyFunc()
    Spy.spying = true
    val is = for (i <- 1 to 2) yield Spy.spy("i", i)
    is shouldBe List(1, 2)
    // you should see messages written to console with "spy" prefix
  }
  it should "not evaluate the argument (noSpy)" in {
    import Spy._
    Spy.spying = true
    var spyMessage: String = ""
    var spySpyMessage: String = ""

    def f(s: => String): String = {spySpyMessage = s; s}

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"explicit spy: $s\n")

    val is = Spy.noSpy(for (i <- 1 to 2) yield Spy.spy(f("i"), i))
    is shouldBe List(1, 2)
    spyMessage shouldBe ""
    spySpyMessage shouldBe ""
  }
  it should "not expand the message when spying is off (global)" in {
    import Spy._
    Spy.spying = false
    var spyMessage: String = ""
    var spySpyMessage: String = ""

    def f(s: => String): String = {spySpyMessage = s; s}

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"explicit spy: $s\n")

    val is = for (i <- 1 to 2) yield Spy.spy(f("i"), i)
    is shouldBe List(1, 2)
    spyMessage shouldBe ""
    spySpyMessage shouldBe ""
  }
  it should "not expand the message when spying is off (local)" in {
    import Spy._
    Spy.spying = true
    var spyMessage: String = ""
    var spySpyMessage: String = ""

    def f(s: => String): String = {spySpyMessage = s; s}

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"explicit spy: $s\n")

    val is = for (i <- 1 to 2) yield Spy.spy(f("i"), i, b = false)
    is shouldBe List(1, 2)
    spyMessage shouldBe ""
    spySpyMessage shouldBe ""
  }
  it should "work with {}-style message" in {
    import Spy._
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"Hello: $s\n")

    val is = for (i <- 1 to 2) yield Spy.spy("{} is the value for i", i)
    is shouldBe List(1, 2)
    spyMessage shouldBe "Hello: 1 is the value for i\nHello: 2 is the value for i\n"
  }
  it should "work with a Success" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"Hello: $s\n")

    Spy.spy("success", Success(1))
    spyMessage shouldBe "Hello: success: Success: 1\n"
  }
  it should "work with a failure" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"Hello: $s\n")

    Spy.spy("failure", Failure(new Exception("junk")))
    spyMessage shouldBe "Hello: failure: Failure(java.lang.Exception: junk)\n"
  }
  it should "work with a Future" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"Hello: $s\n")
    import scala.concurrent.ExecutionContext.Implicits.global
    Spy.spy("success", Future(1))
    spyMessage shouldBe "Hello: success: to be provided in the future\n"
  }
  it should "work with a Stream" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"Hello: $s\n")

    Spy.spy("Stream", Stream(1, 2, 3, 4, 5, 6))
    spyMessage shouldBe "Hello: Stream: [Stream showing at most 5 items] List(1, 2, 3, 4, 5)\n"
  }
  behavior of "Spy.log"
  it should "work with explicit spy func" in {
    // NOTE: we really do need import Spy._ here
    import Spy._
    Spy.spying = true
    var spyMessage: String = ""

    implicit def spyFunc(s: String): Spy = Spy(spyMessage += s"explicit spy: $s\n")

    Spy.log("my log message")
    spyMessage shouldBe "explicit spy: my log message: ()\n"
  }
}
