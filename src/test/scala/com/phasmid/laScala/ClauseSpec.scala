package com.phasmid.laScala

import com.phasmid.laScala.clause.{BoundPredicate, Clause, Truth}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class ClauseSpec extends FlatSpec with Matchers {
  "Truth" should "work for true" in {
    val c = Truth[Nothing](true)
    c() shouldBe true
  }
  it should "work for false" in {
    val c = Truth[Nothing](false)
    c() shouldBe false
  }
  "And" should "work" in {
    (Truth[Unit](true) :& Truth[Unit](true))() shouldBe true
    (Truth[Unit](true) :& Truth[Unit](false))() shouldBe false
    (Truth[Unit](false) :& Truth[Unit](true))() shouldBe false
    (Truth[Unit](false) :& Truth[Unit](false))() shouldBe false
  }
  "Or" should "work" in {
    (Truth[Unit](true) :| Truth[Unit](true))() shouldBe true
    (Truth[Unit](true) :| Truth[Unit](false))() shouldBe true
    (Truth[Unit](false) :| Truth[Unit](true))() shouldBe true
    (Truth[Unit](false) :| Truth[Unit](false))() shouldBe false
  }
  "BoundPredicate" should "work" in {
    val isZero = Func[Int]({_ == 0})
    new BoundPredicate(0, isZero)() shouldBe true
    new BoundPredicate(1, isZero)() shouldBe false
  }
  "Transform" should "work" in {
    val isZero = Func[Int]({_ == 0})
    val p: Clause[Int] = new BoundPredicate(0, isZero)
    val q: Clause[Double] = p transform (_.toDouble,{x: Double => x.toInt})
    p() shouldBe true
  }
  it should "work with lookup function" in {
    val vars = Map("x"->0)
    val p: Clause[String] = new BoundPredicate("x", Func[String](_ == "0"))
    val q: Clause[Int] = p transform(vars.apply _,_.toString)
    q() shouldBe true
  }
}
