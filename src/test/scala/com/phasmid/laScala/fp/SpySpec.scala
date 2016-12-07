package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 8/5/16.
  */
class SpySpec extends FlatSpec with Matchers {
  implicit val logger = Spy.getLogger(getClass)

  behavior of "Spy.spy"
  it should "work with implicit (logger) spy func" in {
    Spy.spying = true
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1,2)
    // you should see log messages written to console (assuming your logging level, i.e. logback-test.xml, is set to DEBUG)
  }
  it should "work with implicit (logger) spy func on the map2 function" in {
    Spy.spying = true
    val x = Some(1)
    FP.map2(x,x)(_+_)
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1,2)
    // you should see log messages written to console (assuming your logging level, i.e. logback-test.xml, is set to DEBUG)
  }
  it should "work with implicit (logger) spy func but with custom logger" in {
    Spy.spying = true
    implicit val logger = org.slf4j.LoggerFactory.getLogger("myLogger")
    (for (i <- 1 to 2) yield Spy.spy("i", i)) shouldBe List(1,2)
    // you should see log messages written to console (assuming your logging level, i.e. logback-test.xml, is set to DEBUG)
  }
  it should "work with explicit spy func" in {
    Spy.spying = true
    var spyMessage: String = ""
    implicit def spyFunc(s: String): Spy = Spy( spyMessage += s"explicit spy: $s\n" )
    val is = for (i <- 1 to 2) yield Spy.spy("i",i)
    is shouldBe List(1,2)
    spyMessage shouldBe "explicit spy: i: 1\nexplicit spy: i: 2\n"
  }
  it should "work with explicit custom println spy func" in {
    implicit def spyFunc(s: String): Spy = Spy(println(s))
    Spy.spying = true
    val is = for (i <- 1 to 2) yield Spy.spy("mySpy: i",i)
    is shouldBe List(1,2)
    // you should see messages written to console with "mySPy" prefix
  }
  it should "work with explicit provided println spy func" in {
    implicit val spyFunc = Spy.getPrintlnSpyFunc()
    Spy.spying = true
    val is = for (i <- 1 to 2) yield Spy.spy("i",i)
    is shouldBe List(1,2)
    // you should see messages written to console with "spy" prefix
  }
  it should "not expand the message when spying is off (global)" in {
    Spy.spying = false
    var spyMessage: String = ""
    var spySpyMessage: String = ""
    def f(s: => String): String = {spySpyMessage = s; s}
    implicit def spyFunc(s: String): Spy = Spy( spyMessage += s"explicit spy: $s\n" )
    val is = for (i <- 1 to 2) yield Spy.spy(f("i"),i)
    is shouldBe List(1,2)
    spyMessage shouldBe ""
    spySpyMessage shouldBe ""
  }
  it should "not expand the message when spying is off (local)" in {
    Spy.spying = true
    var spyMessage: String = ""
    var spySpyMessage: String = ""
    def f(s: => String): String = {spySpyMessage = s; s}
    implicit def spyFunc(s: String): Spy = Spy( spyMessage += s"explicit spy: $s\n" )
    val is = for (i <- 1 to 2) yield Spy.spy(f("i"),i,false)
    is shouldBe List(1,2)
    spyMessage shouldBe ""
    spySpyMessage shouldBe ""
  }
  it should "work with {}-style message" in {
    Spy.spying = true
    var spyMessage: String = ""
    implicit def spyFunc(s: String): Spy = Spy( spyMessage += s"Hello: $s\n" )
    val is = for (i <- 1 to 2) yield Spy.spy("{} is the value for i",i)
    is shouldBe List(1,2)
    spyMessage shouldBe "Hello: 1 is the value for i\nHello: 2 is the value for i\n"
  }
  behavior of "Spy.log"
  it should "work with explicit spy func" in {
    Spy.spying = true
    var spyMessage: String = ""
    implicit def spyFunc(s: String): Spy = Spy( spyMessage += s"explicit spy: $s\n" )
    Spy.log("my log message")
    spyMessage shouldBe "explicit spy: my log message\n"
  }
}
