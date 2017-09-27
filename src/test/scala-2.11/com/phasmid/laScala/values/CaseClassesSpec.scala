/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

// We really do need the following: import com.phasmid.laScala.values.Rational.RationalHelper
import com.phasmid.laScala.RenderableCaseClass
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps


/**
  * @author scalaprof
  *
  * TODO un-ignore these and find out why they don't work with 2.10
  */
class CaseClassesSpec extends FlatSpec with Matchers {

  behavior of "CaseClasses"
  ignore should "work for ordinary case class" in {
    val x = X(1, 2.0)
    val s = RenderableCaseClass.getParameters(x)
    val k1 = s.keys.head
    val k2 = s.keys.tail.head
    k1 should matchPattern { case ("x", _) => }
    k2 should matchPattern { case ("y", _) => }
    s(k1) shouldBe 1
    s(k2) shouldBe 2.0
  }
  ignore should "work for case class with evidence" in {
    val y: Y[Int] = Y[Int](1, 2.0)
    val s = RenderableCaseClass.getParameters(y.asInstanceOf[Y[Any]])
    val k1 = s.keys.head
    val k2 = s.keys.tail.head
    k1 should matchPattern { case ("x", _) => }
    k2 should matchPattern { case ("y", _) => }
    s(k1) shouldBe 1
    s(k2) shouldBe 2.0
  }
}

case class X(x: Int, y: Double)

case class Y[T: Numeric](x: Int, y: Double)
