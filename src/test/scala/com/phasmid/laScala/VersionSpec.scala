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
  val VERSION = "1.0.10"

  println(s"${Version.getVersion}")

  behavior of "next"
  it should "work" in {
    val v = Version(0, None)
    v.next() should matchPattern { case Success(Version(1, None, false)) => }
  }

  behavior of "subversions"
  it should "work" in {
    val v = Version(0, None)
    val stream: Seq[Version[Int]] = v.subversions take 3
    stream shouldBe Seq(Version(0, Some(Version(0, None))), Version(0, Some(Version(1, None))), Version(0, Some(Version(2, None))))
  }

  behavior of "getVersion"
  it should "match version" in {
    Version.getVersion.startsWith(s"lascala $VERSION") shouldBe true
  }
  // The following unit test works correctly, but it requires too much maintenance to have it run all the time
  // Also, it is specific to one Scala binary and therefore, as is, would interfere with the release process
  ignore should "match" in {
    Version.getVersion shouldBe s"lascala $VERSION (compiled with 2.11.8 at 2017-06-03T14:40:43.200 UTC)"
  }
  behavior of "parse"
  it should "decode 1" in {
    val vo = Version.parseLong("1")
    vo should matchPattern { case Some(_) => }
    val v = vo.get
    v.toSeq shouldBe Seq(1L)
    v.toString shouldBe "1"
    v() shouldBe 1L
    v.subversion shouldBe None
  }
  it should "decode 1.1" in {
    val vo = Version.parseLong("1.1")
    vo should matchPattern { case Some(_) => }
    val v = vo.get
    v.toSeq shouldBe Seq(1L, 1L)
    v.toString shouldBe "1.1"
    v() shouldBe 1L
    v.subversion.get() shouldBe 1L
    v.subversion.get.subversion shouldBe None
  }
  it should "decode 1.10" in {
    val vo = Version.parseLong("1.10")
    vo should matchPattern { case Some(_) => }
    val v = vo.get
    v.toSeq shouldBe Seq(1L, 10L)
    v.toString shouldBe "1.10"
    v() shouldBe 1L
    v.subversion.get() shouldBe 10L
    v.subversion.get.subversion shouldBe None
  }
  "next" should "work for LongVersion" in {
    val version = Version(1L)
    version() shouldBe 1L
    version.next() shouldBe Success(Version(2L, None))
    version.subversion shouldBe None
    version.toString shouldBe "1"
  }
}
