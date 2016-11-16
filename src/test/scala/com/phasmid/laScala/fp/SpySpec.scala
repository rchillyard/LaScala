package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

/**
  * Created by scalaprof on 8/5/16.
  */
class SpySpec extends FlatSpec with Matchers {
  behavior of "Spy"
  it should "work with implicit spy func" in {
    Spy.spying = true
    val is = for (i <- 1 to 2) yield Spy.spy("i",i)
    is shouldBe List(1,2)
    // you should see messages written to console
  }
  it should "work with explicit spy func" in {
    Spy.spying = true
    var spyMessage: String = ""
    implicit def spyFunc(s: String): Spy = Spy( spyMessage += s"explicit spy: $s\n" )
    val is = for (i <- 1 to 2) yield Spy.spy("i",i)
    is shouldBe List(1,2)
    spyMessage shouldBe "explicit spy: i: 1\nexplicit spy: i: 2\n"
  }
  it should "not expand the message when spying is off" in {
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
}
