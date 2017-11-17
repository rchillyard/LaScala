/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import java.time.LocalDate

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.language.implicitConversions
import scala.util.{Success, Try}


/**
  * @author scalaprof
  */
class ProbabilitySpec extends FlatSpec with Matchers with Inside {

  behavior of "apply"
  it should "work for favorable and total outcomes" in {
    val evens = Probability(1,2)
    evens() shouldBe 0.5
  }
  it should "work for Rational" in {
    val evens = Probability(Rational(1,2))
    evens() shouldBe 0.5
  }
  it should "work for Double" in {
    val evens = Probability(1.0/2)
    evens() shouldBe 0.5
  }

  behavior of "|"
  it should "work for two evens" in {
    val evens = Probability(1,2)
    (evens | evens) shouldBe Probability.Certain
  }
  it should "work for evens and Impossible" in {
    val evens = Probability(1,2)
    (evens | Probability.Impossible) shouldBe evens
  }
  it should "work for evens and Certain" in {
    val evens = Probability(1,2)
    (evens | Probability.Certain) shouldBe Probability.Certain
  }

  behavior of "&"
  it should "work for two evens" in {
    val evens = Probability(1,2)
    (evens & evens) shouldBe Probability(1,4)
  }
  it should "work for evens and Impossible" in {
    val evens = Probability(1,2)
    (evens & Probability.Impossible) shouldBe Probability.Impossible
  }
  it should "work for evens and Certain" in {
    val evens = Probability(1,2)
    (evens & Probability.Certain) shouldBe evens
  }

  behavior of "!"
  it should "work for evens" in {
    val evens = Probability(1,2)
    (evens !) shouldBe evens
  }
  it should "work for Impossible" in {
    (Probability.Impossible !) shouldBe Probability.Certain
  }
  it should "work for Certain" in {
    (Probability.Certain !) shouldBe Probability.Impossible
  }

  behavior of "*"
  it should "work for evens" in {
    val evens = Probability(1,2)
    (evens * 2.0) shouldBe Probability.Certain
  }
  it should "work for Impossible" in {
    (Probability.Impossible * 1.0) shouldBe Probability.Impossible
  }
  it should "work for Certain" in {
    val evens = Probability(1,2)
    (Probability.Certain * Rational(1,2)) shouldBe evens
  }
}
