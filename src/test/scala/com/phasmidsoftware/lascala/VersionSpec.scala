package com.phasmidsoftware.lascala

/**
  * Created by scalaprof on 7/31/16.
  */

import lascalabuildinfo.BuildInfo
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util._

class VersionSpec extends AnyFlatSpec with Matchers with Inside {
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
    println(Version.getVersion)
    // Derived from BuildInfo rather than a literal: version.sbt is the single source of
    // truth, and a hand-copied mirror here could only ever detect that someone forgot to
    // update the mirror. This tests the actual contract — that getVersion renders name and
    // version in the expected shape.
    Version.getVersion.startsWith(s"${BuildInfo.name} ${BuildInfo.version}") shouldBe true
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
  it should "decode 1.0.11" in {
    val vo = Version.parseLong("1.0.11")
    vo.map(_.toSeq) shouldBe Some(Seq(1L, 0L, 11L))
    vo.map(_.toString) shouldBe Some("1.0.11")
    vo.map(_()) shouldBe Some(1L)
    val subversions = for (v <- vo; v0 <- v.subversion; v11 <- v0.subversion) yield (v0(), v11(), v11.subversion)
    subversions shouldBe Some((0L, 11L, None))
  }
  it should "not decode 1.x or an empty String" in {
    Version.parseLong("1.x") shouldBe None
    Version.parseLong("") shouldBe None
  }
  "next" should "work for LongVersion" in {
    val version = Version(1L)
    version() shouldBe 1L
    version.next() shouldBe Success(Version(2L, None))
    version.subversion shouldBe None
    version.toString shouldBe "1"
  }
}
