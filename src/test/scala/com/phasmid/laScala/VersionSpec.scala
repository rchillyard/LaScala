package com.phasmid.laScala

/**
  * NOTE: you will need to change the value of VERSION before doing a release
  *
  * Created by scalaprof on 7/31/16.
  */

import org.scalatest._

import scala.util._

class VersionSpec extends FlatSpec with Matchers with Inside {
  // TODO you must increment this every time version changes in order for the tests all to run correctly
  val VERSION = "1.0.8"

  println(s"${Version.getVersion}")

  behavior of "getVersion"
  it should "match version" in {
    Version.getVersion.startsWith(s"lascala $VERSION") shouldBe true
  }
  // The following unit test works correctly, but it requires too much maintenance to have it run all the time
  // Also, it is specific to one Scala binary and therefore, as is, would interfere with the release process
  ignore should "match" in {
    Version.getVersion shouldBe s"lascala $VERSION (compiled with 2.11.8 at 2017-06-03T14:40:43.200 UTC)"
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
