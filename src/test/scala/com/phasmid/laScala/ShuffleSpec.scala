package com.phasmid.laScala

/**
  * Created by scalaprof on 7/31/16.
  */

import org.scalatest._

import scala.collection.immutable.IndexedSeq
import scala.language.postfixOps

class ShuffleSpec extends FlatSpec with Matchers with Inside {
  "Shuffle" should "work for an List of 5 integers" in {
    Shuffle(0L)(Seq(1, 2, 3, 4, 5)) shouldBe Seq(5, 3, 2, 4, 1)
  }
  it should "work for an List of 5 Strings" in {
    Shuffle(0L)(Seq("the", "quick", "brown", "fox", "jumps")) shouldBe Seq("jumps", "brown", "quick", "fox", "the")
  }
  it should "work for an IndexedSeq of 5 integers" in {
    Shuffle(0L)(IndexedSeq(1, 2, 3, 4, 5)) shouldBe IndexedSeq(5, 3, 2, 4, 1)
  }
  it should "work for an IndexedSeq of 5 Strings" in {
    Shuffle(0L)(IndexedSeq("the", "quick", "brown", "fox", "jumps")) shouldBe IndexedSeq("jumps", "brown", "quick", "fox", "the")
  }
}
