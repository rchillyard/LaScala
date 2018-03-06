/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fuzzy

import org.scalatest.{FlatSpec, Matchers}

import scala.math.Numeric.DoubleIsFractional
import scala.reflect.ClassTag

/**
  * Created by scalaprof on 4/7/17.
  */
class ProbabilitySpec extends FlatSpec with Matchers {
  behavior of "step"

  it should "apply correctly" in {
    val x = Probability.step(0.0)
    x(1.0) shouldBe 1.0
    x(-1.0) shouldBe 0.0
    x(0) shouldBe 1.0
  }

  behavior of "bounded"

  it should "apply correctly" in {
    val x = Probability.bounded(-1.0,1.0)
    x(1.0) shouldBe 0.5 +- 0.00000001
  }

}
