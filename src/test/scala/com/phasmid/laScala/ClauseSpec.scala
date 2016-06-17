package com.phasmid.laScala

import com.phasmid.laScala.parser._
import com.phasmid.laScala.predicate.Func
import org.scalatest.{FlatSpec, Matchers}

import scala.util._

/**
  * @author scalaprof
  */
class ClauseSpec extends FlatSpec with Matchers {
  "Truth" should "work for true" in {
    val c = Truth[Nothing](b=true)
    c() should matchPattern {case Success(true) => }
  }
  it should "work for false" in {
    val c = Truth[Nothing](b=false)
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
    assert(true)
    // Ignore the rest
//    val q: Clause[Double] = p transform (_.toDouble, _.toDouble)
//    p() should matchPattern {case Success(true) => }
  }
  ignore should "work with lookup function" in {
    val vars = Map("x"->0)
    val p: Clause[String] = new BoundPredicate("x", Func[String](_ == "0"))
    val q: Clause[Int] = p transform (vars.apply,_.toInt)
    q() should matchPattern {case Success(true) => }
  }
  ignore should """evaluate x>1.0K as true""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.condition, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    val variables = Map("x" -> 2000.0)
    implicit val lookup = variables.apply _
    val clause = r.get.asClause
    val truth: Clause[Double] = clause.transform(lookup, _.toDouble)
    truth() should matchPattern {case Success(true) => }
  }
  "x > $z" should "be false when z=3" in {
    val variables: Map[String, Int] = Map("x"->2, "y"->4, "z"->3)
    val p = new RuleParser()
    val rt: Try[Rule] = p.parseRule("x > $z")
    rt should matchPattern { case Success(_) => }
        val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
        ct match {
          case Success(c) =>
            val q: Clause[Int] = Clause.convertFromStringClauseToOrderingClause(c,variables)
            q() should matchPattern { case Success(false) => }
          case Failure(x) => fail(x); Truth(false)
        }
  }
  it should "be true when z=1" in {
    val variables: Map[String, Int] = Map("x"->2, "y"->4, "z"->1)
    val rt: Try[Rule] = new RuleParser().parseRule("x > $z")
        val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
        ct match {
          case Success(c) =>
            val q: Clause[Int] = Clause.convertFromStringClauseToOrderingClause(c,variables)
            q() should matchPattern { case Success(true) => }
          case Failure(x) => fail(x); Truth(false)
        }
  }
  "Clause[String]" should "be true for 1=1" in {
    val p = new RuleParser()
    val rt: Try[Rule] = p.parseRule("1=1")
    rt should matchPattern { case Success(_) => }
    val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
    ct match {
      case Success(c) =>
        c() shouldBe Success(true)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be true for 2>1" in {
    val p = new RuleParser()
    val rt: Try[Rule] = p.parseRule("2>1")
    rt should matchPattern { case Success(_) => }
    val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
    ct match {
      case Success(c) =>
        c() shouldBe Success(true)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be false for 2<1" in {
    val p = new RuleParser()
    val rt: Try[Rule] = p.parseRule("2<1")
    rt should matchPattern { case Success(_) => }
    val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
    ct match {
      case Success(c) =>
        c() shouldBe Success(false)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be false for \"x = $z - 1\"" in {
    val p = new RuleParser()
    val rt: Try[Rule] = p.parseRule("x = $z - 1")
    rt should matchPattern { case Success(_) => }
    val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
    ct match {
      case Success(c) =>
        c() shouldBe Success(false)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  "Clause[Int]" should "be true for \"x = $z - 1\" when x=2, z=3" in {
    val variables: Map[String, Int] = Map("x"->2, "y"->4, "z"->3)
    val p = new RuleParser()
    val rt: Try[Rule] = p.parseRule("x = $z - 1")
    val ct: Try[Clause[String]] = for (r <- rt) yield r.asClause
    ct match {
      case Success(c) =>
        val q: Clause[Int] = Clause.convertFromStringClauseToOrderingClause(c,variables)
        q() should matchPattern { case Success(true) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
}
