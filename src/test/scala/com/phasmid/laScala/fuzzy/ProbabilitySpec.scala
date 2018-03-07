/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fuzzy

import com.phasmid.laScala.values.FiniteIntegral.IntIsFiniteIntegral
import com.phasmid.laScala.values.Rational
import org.scalatest.{FlatSpec, Matchers}

import scala.math.Numeric.DoubleIsFractional

/**
  * Created by scalaprof on 4/7/17.
  */
class ProbabilitySpec extends FlatSpec with Matchers {

  behavior of "coin flip"

  it should "apply correctly" in {
    val x = Probability.coinFlip
    x(true) shouldBe Rational.half
    x(false) shouldBe Rational.half
  }

  it should "pdf correctly" in {
    val x = Probability.coinFlip
    x.isPdf shouldBe false
  }

//  it should "& correctly" in {
//    val x: Probability[Boolean, Rational[Int]] = Probability.coinFlip
//    val y = x.!
//    val z = x & y
//    z(true) shouldBe Rational.zero
//    z(false) shouldBe Rational.zero
//  }

  behavior of "step"

  it should "apply correctly Double" in {
    val x = Probability.step(0.0)
    x(1.0) shouldBe 1.0
    x(-1.0) shouldBe 0.0
    x(0) shouldBe 1.0
  }

  it should "apply correctly Int" in {
    val x = Probability.step(0)
    x(1) shouldBe 1
    x(-1) shouldBe 0
    x(0) shouldBe 1
  }

  it should "pdf correctly" in {
    val x = Probability.step(0.0)
    x.isPdf shouldBe true
  }

  it should "! correctly" in {
    val x: Probability[Int, Int] = Probability.step(0).!
    x(1) shouldBe 0
    x(0) shouldBe 1
    x(-1) shouldBe 1
  }

  behavior of "bounded"

  it should "apply correctly" in {
    val x = Probability.bounded(-1.0,1.0)
    x(-2.0) shouldBe 0.0 +- 0.00000001
    x(-1.0) shouldBe 0.5 +- 0.00000001
    x(0.0) shouldBe 0.5 +- 0.00000001
    x(1.0) shouldBe 0.5 +- 0.00000001
    x(2.0) shouldBe 0.0 +- 0.00000001
  }

  it should "! correctly" in {
    val x = Probability.bounded(-1.0,1.0).!
    x(-2.0) shouldBe 1.0 +- 0.00000001
    x(-1.0) shouldBe 0.5 +- 0.00000001
    x(0.0) shouldBe 0.5 +- 0.00000001
    x(1.0) shouldBe 0.5 +- 0.00000001
    x(2.0) shouldBe 1.0 +- 0.00000001
  }

  it should "& correctly" in {
    import Rational.{RationalHelper, RationalIsFractionalInt, RationalIsInt}
    val x: Probability[Double, Rational[Int]] = Probability.bounded[Double, Rational[Int]](-1,1)
      val z = x & x
    val y = z(1)
    println(s"$y ${y.getClass}")
      z(1) shouldBe r"1/4"
      z(1) shouldBe r"1/4"
    }
}
