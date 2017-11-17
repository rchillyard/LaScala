/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.language.implicitConversions


/**
  * @author scalaprof
  */
class FuzzySpec extends FlatSpec with Matchers with Inside {

  behavior of "apply"
  it should "work for simple bounded" in {
    val two = Bounded(2.0,1.0)
    two() shouldBe 2
  }

  behavior of "p"
  it should "work for simple bounded" in {
    val two = Bounded(2.0,1.0)
    two.p(2) shouldBe Probability(1,2)
    two.p(4) shouldBe Probability.Impossible
  }
}
