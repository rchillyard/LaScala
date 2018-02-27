/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}

import scala.math.Numeric.DoubleIsFractional

/**
  * Created by scalaprof on 4/7/17.
  */
class FuzzySpec extends FlatSpec with Matchers {
  behavior of "Exact"

  trait DiscreteInt extends Discrete[Int] {
    override def fractional: Fractional[Int] = throw new FuzzyException("Int is not fractional")

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
    val sb = new StringBuilder
    x foreach (t => sb.append(t))
    sb.toString shouldBe "1"
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

  behavior of "Bounded"

  trait DiscreteDouble extends Discrete[Double] {
    override def fractional: Fractional[Double] = DoubleIsFractional

    override def isDiscrete(t: Double): Boolean = false
  }

  implicit object DiscreteDouble extends DiscreteDouble

  it should "be non-exact" in {
    val x = Bounded(1,0.1)
    x.isExact shouldBe false
  }
  it should "not support get" in {
    val x = Bounded(1,0.1)
    // TODO this should throw an exception
//    x.get shouldBe 1
  }
  // TODO reinstate this
  ignore should "support pdf" in {
    val x = Bounded(1,0.1)
    x.pdf(1.0) shouldBe 0.5 +- 0.00000001
    x.pdf(-1.0) shouldBe 0.5 +- 0.00000001
    x.pdf(0.0) shouldBe 0.5 +- 0.00000001
    x.pdf(2.0) shouldBe 0.0 +- 0.00000001
    x.pdf(-2.0) shouldBe 0.0 +- 0.00000001
  }
  it should "support foreach" in {
    val x = Bounded(1,0.1)
    x foreach println
  }
  it should "support unit" in {
    val x = Bounded(0,0.0)
    val y = x unit 1.0
    y.isExact shouldBe false
  }
  it should "support flatMap" in {
    val x = Bounded(1,0.1)
    val y = x flatMap (t => Exact(t*2))
    y.isExact shouldBe true
    y.get shouldBe 2
  }
  it should "support map" in {
    val x = Bounded(1,0.1)
    val y = x map (t => t*2)
    y.isExact shouldBe false
  }

}
