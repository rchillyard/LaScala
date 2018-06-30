/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fuzzy

import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.ClassTag

/**
  * Created by scalaprof on 4/7/17.
  */
class FuzzySpec extends FlatSpec with Matchers {
  behavior of "Exact"

//  trait DiscreteInt extends Discrete[Int] {
//    def convertTo[U: ClassTag](t: Int): U = 0.asInstanceOf[U] // FIXME
//
//    override def fractional: Fractional[Int] = throw new FuzzyException("Int is not fractional")
//
//    override def isDiscrete(t: Int): Boolean = true
//  }
//
//  implicit object DiscreteInt extends DiscreteInt

  it should "be exact" in {
    val x = Exact(1)
    x.isExact shouldBe true
  }
  it should "support get" in {
    val x = Exact(1)
    x() shouldBe Some(1)
  }
  it should "support pdf" in {
    val x = Exact(1)
    x.fuzziness should matchPattern { case None => }
  }
//  it should "support foreach" in {
//    val x = Exact(1)
//    val sb = new StringBuilder
//    x foreach (t => sb.append(t))
//    sb.toString shouldBe "1"
//  }
//  it should "support unit" in {
//    val x = Exact(0)
//    val y = x unit 1
//    y.isExact shouldBe true
//    y.get shouldBe 1
//  }
//  it should "support flatMap" in {
//    val x = Exact(1)
//    val y = x flatMap (t => Exact(t*2))
//    y.isExact shouldBe true
//    y.get shouldBe 2
//  }
//  it should "support map" in {
//    val x = Exact(1)
//    val y = x map (t => t*2)
//    y.isExact shouldBe true
//    y.get shouldBe 2
//  }
//
  behavior of "Bounded"

  // TODO fix these problems which have to do with Rational/FiniteIntegral
  ignore should "be non-exact" in {
    val x = Bounded(1,0.1)
    x.isExact shouldBe false
  }
  ignore should "not support get" in {
    val x = Bounded(1,0.1)
    // TODO this should throw an exception
//    x.get shouldBe 1
  }
  ignore should "support fuzziness" in {
    val po = for (f <- Bounded(1, 0.1).fuzziness) yield f()
    (for (p <- po) yield p(1.0)).get shouldBe 5.0 +- 0.0000001
    (for (p <- po) yield p(-1.0)).get shouldBe 0.0
    (for (p <- po) yield p(0.0)).get shouldBe 5.0 +- 0.0000001
    (for (p <- po) yield p(2.0)).get shouldBe 5.0 +- 0.0000001
    (for (p <- po) yield p(3.0)).get shouldBe 0.0 +- 0.0000001

//    x.pdf(1.0) shouldBe 0.5 +- 0.00000001
//    x.pdf(-1.0) shouldBe 0.5 +- 0.00000001
//    x.pdf(0.0) shouldBe 0.5 +- 0.00000001
//    x.pdf(2.0) shouldBe 0.0 +- 0.00000001
//    x.pdf(-2.0) shouldBe 0.0 +- 0.00000001
  }
//  it should "support foreach" in {
//    val x = Bounded(1,0.1)
//    x foreach println
//  }
//  it should "support unit" in {
//    val x = Bounded(0,0.0)
//    val y = x unit 1.0
//    y.isExact shouldBe false
//  }
//  it should "support flatMap" in {
//    val x = Bounded(1,0.1)
//    val y = x flatMap (t => Exact(t*2))
//    y.isExact shouldBe true
//    y.get shouldBe 2
//  }
//  it should "support map" in {
//    val x = Bounded(1,0.1)
//    val y = x map (t => t*2)
//    y.isExact shouldBe false
//  }

}
