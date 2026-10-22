/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala.values

// We really do need the following: import com.phasmidsoftware.lascala.values.Rational.RationalHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class Rational2Spec extends AnyFlatSpec with Matchers {

  "toString" should "be decimal when exact" in {
    val r = Rational(1, 2)
    r.toString() should be("0.5")
  }
  it should "be rational when not exact: 2/3" in {
    val r = Rational(2, 3)
    r.toString() should be("2/3")
  }
}