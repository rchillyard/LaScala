package com.phasmid.laScala

import com.phasmid.laScala.clause.{BoundPredicate, Clause, Truth}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
  * @author scalaprof
  */
class ClauseSpec extends FlatSpec with Matchers {
  "Truth" should "work for true" in {
    val c = Truth[Nothing](true)
    c() should matchPattern {case Success(true) => }
  }
  it should "work for false" in {
    val c = Truth[Nothing](false)
    c() should matchPattern {case Success(false) => }
  }
  "And" should "work" in {
    (Truth[Unit](true) :& Truth[Unit](true))() should matchPattern {case Success(true) => }
    (Truth[Unit](true) :& Truth[Unit](false))() should matchPattern {case Success(false) => }
    (Truth[Unit](false) :& Truth[Unit](true))() should matchPattern {case Success(false) => }
    (Truth[Unit](false) :& Truth[Unit](false))() should matchPattern {case Success(false) => }
  }
  "Or" should "work" in {
    (Truth[Unit](true) :| Truth[Unit](true))() should matchPattern {case Success(true) => }
    (Truth[Unit](true) :| Truth[Unit](false))() should matchPattern {case Success(true) => }
    (Truth[Unit](false) :| Truth[Unit](true))() should matchPattern {case Success(true) => }
    (Truth[Unit](false) :| Truth[Unit](false))() should matchPattern {case Success(false) => }
  }
  "BoundPredicate" should "work" in {
    val isZero = Func[Int]({_ == 0})
    new BoundPredicate(0, isZero)() should matchPattern {case Success(true) => }
    new BoundPredicate(1, isZero)() should matchPattern {case Success(false) => }
  }
  "Transform" should "work" in {
    val isZero = Func[Int]({_ == 0})
    val p: Clause[Int] = new BoundPredicate(0, isZero)
    val q: Clause[Double] = p transform (_.toDouble,{x: Double => x.toInt})
    p() should matchPattern {case Success(true) => }
  }
  it should "work with lookup function" in {
    val vars = Map("x"->0)
    val p: Clause[String] = new BoundPredicate("x", Func[String](_ == "0"))
    val q: Clause[Int] = p transform(vars.apply _,_.toString)
    q() should matchPattern {case Success(true) => }
  }
}
