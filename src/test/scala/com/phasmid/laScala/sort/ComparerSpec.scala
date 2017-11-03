/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.sort

import com.phasmid.laScala.sort.Comparison._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class ComparerSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Comparison"

  it should "apply(Int)" in {
    Comparison(0) shouldBe Same
    Comparison(1) shouldBe more
    Comparison(-1) shouldBe less
  }
  it should "toInt" in {
    Comparison(-1).toInt shouldBe -1
    Comparison(0).toInt shouldBe 0
    Comparison(1).toInt shouldBe 1
  }
  it should "apply(Option[Boolean])" in {
    Comparison(None) shouldBe Same
    Comparison(Some(false)) shouldBe more
    Comparison(Some(true)) shouldBe less
  }
  it should "flip" in {
    more.flip shouldBe less
    less.flip shouldBe more
    Same.flip shouldBe Same
  }
  ignore should "orElse" in {
    more orElse more shouldBe more
    more orElse less shouldBe more
    less orElse more shouldBe less
    less orElse less shouldBe less
    Same orElse less shouldBe less
    Same orElse more shouldBe more
    Same orElse Same shouldBe Same
  }

  case class Composite(i: Int, s: String)

  object Composite {

    object OrderingCompositeInt extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.i.compare(y.i)
    }

    object OrderingCompositeString extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.s.compare(y.s)
    }

  }

  behavior of "Comparer"

  it should "compare Ints (1)" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer(1, 2) shouldBe less
    comparer(1, 1) shouldBe Same
    comparer(2, 1) shouldBe more
  }
  it should "evaluate operators on Int" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer.>(1, 2) shouldBe false
    comparer.>(1, 1) shouldBe false
    comparer.>(2, 1) shouldBe true
    comparer.<(1, 2) shouldBe true
    comparer.<(1, 1) shouldBe false
    comparer.<(2, 1) shouldBe false
    comparer.<=(1, 2) shouldBe true
    comparer.<=(1, 1) shouldBe true
    comparer.<=(2, 1) shouldBe false
    comparer.>=(1, 2) shouldBe false
    comparer.>=(1, 1) shouldBe true
    comparer.>=(2, 1) shouldBe true
    comparer.==(1, 2) shouldBe false
    comparer.==(1, 1) shouldBe true
    comparer.==(2, 1) shouldBe false
    comparer.!=(1, 2) shouldBe true
    comparer.!=(1, 1) shouldBe false
    comparer.!=(2, 1) shouldBe true
  }
  it should "compose with function" in {
    val comparer: Comparer[Int] = Comparer.intComparer.compose(_ flip)
    comparer((1, 2)) shouldBe Comparison.more
  }
  it should "invert" in {
    val comparer = Comparer.intComparer.invert
    comparer((1, 2)) shouldBe Comparison.more
  }
  ignore should "compose with Comparer" in {
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer3 = comparer1 orElse comparer2
    comparer3(Composite(1, "a"), Composite(1, "z")) shouldBe less
    comparer3(Composite(1, "a"), Composite(2, "a")) shouldBe less
    comparer3(Composite(1, "z"), Composite(2, "a")) shouldBe more
    comparer3(Composite(1, "a"), Composite(1, "a")) shouldBe Same
    comparer3(Composite(2, "a"), Composite(1, "a")) shouldBe more
    comparer3(Composite(1, "z"), Composite(1, "a")) shouldBe more
    val comparer4 = comparer2 orElse comparer1
    comparer4(Composite(1, "a"), Composite(1, "z")) shouldBe less
    comparer4(Composite(1, "a"), Composite(2, "a")) shouldBe less
    comparer4(Composite(1, "z"), Composite(2, "a")) shouldBe less
    comparer4(Composite(1, "a"), Composite(1, "a")) shouldBe Same
    comparer4(Composite(2, "a"), Composite(1, "a")) shouldBe more
    comparer4(Composite(1, "z"), Composite(1, "a")) shouldBe more
  }
}