package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 10/19/16.
  */
class MPTTSpec extends FlatSpec with Matchers {

  behavior of "MPTTEntry.contains"
  it should "be true for (0,3).contains((1,2)" in {
    val m1 = MPTTEntry("A")(0,3)
    val m2 = MPTTEntry("Aardvark")(1,2)
    m1.contains(m2) shouldBe true
  }
  it should "be true for (0,1).contains((0,1)" in {
    val m1 = MPTTEntry("A")(0,1)
    m1.contains(m1) shouldBe true
  }
  it should "be false for (1,2).contains((0,3)" in {
    val m1 = MPTTEntry("A")(0,3)
    val m2 = MPTTEntry("Aardvark")(1,2)
    m2.contains(m1) shouldBe false
  }
}
