package com.phasmid.laScala

/**
  * Created by scalaprof on 7/31/16.
  */

import org.scalatest._

import scala.util._

class VersionSpec extends FlatSpec with Matchers with Inside {
  behavior of "getVersion"
  // The following unit test works, but it requires too much maintenance to have it run all the time
  ignore should "match" in {
    Version.getVersion shouldBe "lascala 1.0.3-SNAPSHOT (compiled with 2.11.8 at 1496320258395)"
  }
  "apply" should "decode 1.1" in {
    val vo = LongVersion.parse("1.1")
    vo should matchPattern { case Some(_) => }
    val v = vo.get
    v.toSeq shouldBe Seq(1L, 1L)
    v.toString shouldBe "1.1"
    v.get shouldBe 1L
    v.subversion.get.get shouldBe 1L
    v.subversion.get.subversion shouldBe None
  }
  "next" should "work for LongVersion" in {
    val version = LongVersion(1)
    version.get shouldBe 1L
    version.next shouldBe Success(LongVersion(2))
    version.subversion shouldBe None
    version.toString shouldBe "1"
  }
  "with" should "work for LongVersion" in {
    val v1 = LongVersion(1)
    val v2 = v1.withSubversion(1)
    v2 match {
      case Success(s) =>
        s.get shouldBe 1L
        s.toString shouldBe "1.1"
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }
}
