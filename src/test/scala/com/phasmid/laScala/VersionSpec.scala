package com.phasmid.laScala

/**
  * Created by scalaprof on 7/31/16.
  */

import org.scalatest._

import scala.collection.immutable.IndexedSeq
import scala.language.postfixOps
import scala.util.Success

class VersionSpec extends FlatSpec with Matchers with Inside {
  "next" should "work for LongVersion" in {
    val v1 = LongVersion(1)
    v1.tag shouldBe 1L
    v1.next("") shouldBe Success(LongVersion(2))
  }
}
