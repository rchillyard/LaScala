package com.phasmid.laScala

import java.time.LocalDate

import com.phasmid.laScala.parser._
import com.phasmid.laScala.predicate.Func
import com.phasmid.laScala.values.Value
import org.scalatest.{FlatSpec, Matchers}

import scala.util._

/**
  * @author scalaprof
  */
class RuleSpec extends FlatSpec with Matchers {
  "Truth" should "work for true" in {
    val c = Truth[Nothing](b = true)
    c() should matchPattern { case Success(true) => }
  }
  it should "work for false" in {
    val c = Truth[Nothing](b = false)
    c() should matchPattern { case Success(false) => }
  }
  "And" should "work" in {
    (Truth[Unit](b = true) :& Truth[Unit](b = true)) () should matchPattern { case Success(true) => }
    (Truth[Unit](b = true) :& Truth[Unit](b = false)) () should matchPattern { case Success(false) => }
    (Truth[Unit](b = false) :& Truth[Unit](b = true)) () should matchPattern { case Success(false) => }
    (Truth[Unit](b = false) :& Truth[Unit](b = false)) () should matchPattern { case Success(false) => }
  }
  "Or" should "work" in {
    (Truth[Unit](b = true) :| Truth[Unit](b = true)) () should matchPattern { case Success(true) => }
    (Truth[Unit](b = true) :| Truth[Unit](b = false)) () should matchPattern { case Success(true) => }
    (Truth[Unit](b = false) :| Truth[Unit](b = true)) () should matchPattern { case Success(true) => }
    (Truth[Unit](b = false) :| Truth[Unit](b = false)) () should matchPattern { case Success(false) => }
  }
  "BoundPredicate" should "work" in {
    val isZero = Func[Int]({
      _ == 0
    })
    new BoundPredicate(0, isZero)() should matchPattern { case Success(true) => }
    new BoundPredicate(1, isZero)() should matchPattern { case Success(false) => }
  }
  "Transform" should "work" in {
    val isZero = Func[Int]({
      _ == 0
    })
    val _: Rule[Int] = new BoundPredicate(0, isZero)
    assert(true)
    // XXX Ignore the rest
    //    val q: Rule[Double] = p transform (_.toDouble, _.toDouble)
    //    p() should matchPattern {case Success(true) => }
  }
  // CONSIDER why this is being ignored
  ignore should "work with lookup function" in {
    val vars = Map("x" -> 0)
    val p: Rule[String] = new BoundPredicate("x", Func[String](_ == "0"))
    val q: Rule[Int] = p transform(vars.apply, _.toInt)
    q() should matchPattern { case Success(true) => }
  }
  ignore should """evaluate x>1.0K as true""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.condition, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    val variables = Map("x" -> 2000.0)
    implicit val lookup = variables.apply _
    val rule = r.get.asRule
    val truth: Rule[Double] = rule.transform(lookup, _.toDouble)
    truth() should matchPattern { case Success(true) => }
  }
  "x > $z" should "be false when z=3" in {
    val variables: Map[String, Int] = Map("x" -> 2, "y" -> 4, "z" -> 3)
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("x > $z")
    rlt should matchPattern { case Success(_) => }
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[Int]] = Rule.convertFromStringRuleToValuableRule(rt, variables.get)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(false) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be true when z=1" in {
    val variables: Map[String, Int] = Map("x" -> 2, "y" -> 4, "z" -> 1)
    val rlt: Try[RuleLike] = new RuleParser().parseRule("x > $z")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[Int]] = Rule.convertFromStringRuleToValuableRule(rt, variables.get)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(true) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  "Rule[String]" should "be true for 1=1" in {
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("1=1")
    rlt should matchPattern { case Success(_) => }
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    rt match {
      case Success(r) =>
        r() shouldBe Success(true)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be true for 2>1" in {
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("2>1")
    rlt should matchPattern { case Success(_) => }
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    rt match {
      case Success(r) =>
        r() shouldBe Success(true)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be false for 2<1" in {
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("2<1")
    rlt should matchPattern { case Success(_) => }
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    rt match {
      case Success(r) =>
        r() shouldBe Success(false)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be false for \"x = $z - 1\"" in {
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("x = $z - 1")
    rlt should matchPattern { case Success(_) => }
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    rt match {
      case Success(r) =>
        r() shouldBe Success(false)
      case Failure(x) => fail(x); Truth(false)
    }
  }
  "Rule[Int]" should "be true for \"x = $z - 1\" when x=2, z=3" in {
    val variables: Map[String, Int] = Map("x" -> 2, "y" -> 4, "z" -> 3)
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("x = $z - 1")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[Int]] = Rule.convertFromStringRuleToValuableRule(rt, variables.get)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(true) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be true for \"x in 1...$z\" when x=2, z=3" in {
    val variables: Map[String, Int] = Map("x" -> 2, "y" -> 4, "z" -> 3)
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("x in 1 ... $z")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[Int]] = Rule.convertFromStringRuleToValuableRule(rt, variables.get)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(true) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be true for \"x in $y*2 ... $z*2\" when x=6, y=2, z=3" in {
    val variables: Map[String, Int] = Map("x" -> 6, "y" -> 2, "z" -> 3)
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("x in $y*2 ... $z*2")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[Int]] = Rule.convertFromStringRuleToValuableRule(rt, variables.get)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(true) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be false for \"x in $y*2 ... $z*2\" when x=6, y=4, z=3" in {
    val variables: Map[String, Int] = Map("x" -> 6, "y" -> 4, "z" -> 3)
    val p = new RuleParser()
    val rlt: Try[RuleLike] = p.parseRule("x in $y*2 ... $z*2")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[Int]] = Rule.convertFromStringRuleToValuableRule(rt, variables.get)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(false) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  "Orderable rule" should "be true" in {
    implicit val pattern = ""
    val dates: Map[String, Any] = Map("x" -> "2016-07-13", "z" -> "2015-07-31")
    val values: Map[String, Value] = Value.sequence(dates)
    val variables = for ((k, v) <- values) yield (k, v.asOrderable[LocalDate])
    val rlt: Try[RuleLike] = new RuleParser().parseRule("x > $z")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[LocalDate]] = Rule.convertFromStringRuleToOrderableRule(rt, variables.apply)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(true) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }
  it should "be false" in {
    implicit val pattern = "MMM dd, yyyy"
    val dates: Map[String, Any] = Map("x" -> "Jul 13, 2016", "z" -> "Jul 31, 2015")
    val values: Map[String, Value] = Value.sequence(dates)
    val variables = for ((k, v) <- values) yield (k, v.asOrderable[LocalDate])
    val rlt: Try[RuleLike] = new RuleParser().parseRule("x < $z")
    val rt: Try[Rule[String]] = for (r <- rlt) yield r.asRule
    val qt: Try[Rule[LocalDate]] = Rule.convertFromStringRuleToOrderableRule[LocalDate](rt, variables.apply)
    qt match {
      case Success(q) =>
        q() should matchPattern { case Success(false) => }
      case Failure(x) => fail(x); Truth(false)
    }
  }

}
