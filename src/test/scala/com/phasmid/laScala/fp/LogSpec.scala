package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

/**
  * Created by scalaprof on 9/7/16.
  */
class LogSpec extends FlatSpec with Matchers {

  import Log._

  behavior of "Log"

  it should "log Int" in {
    1.log("test") shouldBe 1
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
}
