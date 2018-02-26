/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 4/7/17.
  */
class FuzzySpec extends FlatSpec with Matchers {
  behavior of "Exact"

  trait DiscreteInt extends Discrete[Int] {
    override def isDiscrete(t: Int): Boolean = true
  }

  implicit object DiscreteInt extends DiscreteInt

  it should "be exact" in {
    val x = Exact(1)
    x.isExact shouldBe true
  }
  it should "support get" in {
    val x = Exact(1)
    x.get shouldBe 1
  }
  it should "support pdf" in {
    val x = Exact(1)
    x.pdf(1) shouldBe 1.0 +- 0.00000001
  }
  it should "support foreach" in {
    val x = Exact(1)
    x foreach println
    x.isExact shouldBe true
    x.get shouldBe 1
  }
  it should "support unit" in {
    val x = Exact(0)
    val y = x unit 1
    y.isExact shouldBe true
    y.get shouldBe 1
  }
  it should "support flatMap" in {
    val x = Exact(1)
    val y = x flatMap (t => Exact(t*2))
    y.isExact shouldBe true
    y.get shouldBe 2
  }
  it should "support map" in {
    val x = Exact(1)
    val y = x map (t => t*2)
    y.isExact shouldBe true
    y.get shouldBe 2
  }

}
