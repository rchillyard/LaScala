/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

// We really do need the following: import com.phasmid.laScala.values.Rational.RationalHelper
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class Rational2Spec extends FlatSpec with Matchers {

  "toString" should "be decimal when exact" in {
    val r = Rational(1, 2)
    r.toString() should be("0.5")
  }
  it should "be rational when not exact: 2/3" in {
    val r = Rational(2, 3)
    r.toString() should be("2/3")
  }
}