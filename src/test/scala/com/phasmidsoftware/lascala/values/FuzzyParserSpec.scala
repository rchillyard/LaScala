/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala.values

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import scala.util._


/**
  * @author scalaprof
  */
class FuzzyParserSpec extends AnyFlatSpec with Matchers with Inside {

  val parser = new FuzzyParser

  behavior of "apply"
  it should "work for exact" in {
    val target = parser.parseFuzzy[Double]("2.0")
    target should matchPattern { case Success(_) => }
    val f = target.get
    f shouldBe Exact(2.0)
  }
  it should "work for Bounded" in {
    val target = parser.parseFuzzy[Double]("2.0~")
    target should matchPattern { case Success(_) => }
    val f = target.get
    f shouldBe Bounded(2.0, 0.05)
  }
  it should "work for Gaussian" in {
    val target = parser.parseFuzzy[Double]("2.0(1)")
    target should matchPattern { case Success(_) => }
    val f = target.get
    f shouldBe Gaussian(2.0, 0.1)
  }

}
