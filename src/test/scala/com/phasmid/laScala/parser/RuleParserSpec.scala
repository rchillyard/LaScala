package com.phasmid.laScala.parser

import com.phasmid.laScala.clause.Clause
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
 * @author scalaprof
 */
class RuleParserSpec extends FlatSpec with Matchers {
  "value" should """parse 1 as Number("1","1")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Number("1","1") => }
  }
  it should """parse 1.0K as Number("1.0","K")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Number("1.0","K") => }
  }
  it should """parse 1.0@m as Number("1.0","m")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "1.0@m")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Number("1.0","m") => }
  }
  it should """parse $x as Variable(x)""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "$x")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Variable("x") => }
  }
  it should """parse ${x.y} as Variable(x.y)""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "${x.y}")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Variable("x.y") => }
  }
  it should """evaluate 1 as 1""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case n: Number => val x: Int = n; x shouldBe 1
      case v: Variable => fail
    }
  }
  it should """evaluate 1.0K as 1000.0""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case n: Number => val x: Double = n; x === 1000.0 +- 0.001
      case v: Variable => fail
    }
  }
  "predicate" should """parse >1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.predicate, ">1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case PredicateExpr(">",Number("1.0","K")) => }
  }
  "condition" should """parse x>1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.condition, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    val rule = r.get
    rule should matchPattern { case Condition(Variable("x"),PredicateExpr(">",Number("1.0","K"))) => }
  }
  it should """evaluate x>1.0K as true""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.condition, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    val rule = r.get
    val variables = Map("x" -> 2000.0)
    implicit val lookup = variables.apply _
    val clause = rule.asClause
    val truth: Clause[Double] = clause.transform(lookup,_.toString)
    truth() should matchPattern {case Success(true) => }
  }
  "factor" should """parse x>1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition(Variable("x"),PredicateExpr(">",Number("1.0","K"))) => }
  }
  it should """parse (x>1.0K) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "(x>1.0K)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Parentheses(Disjunction(List(Conjunction(List(Condition(Variable("x"),PredicateExpr(">",Number("1.0","K")))))))) => }
  }
  it should """parse x>1 as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition(Variable("x"),PredicateExpr(">",Number("1","1"))) => }
  }
  "term" should """parse x>1 & x<3 as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.term, "x>1 & x<3")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Conjunction(List(Condition(Variable("x"),PredicateExpr(">",Number("1","1"))), Condition(Variable("x"),PredicateExpr("<",Number("3","1"))))) => }
  }
  it should """evaluate x>1 & x<3 as true""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.term, "x>1 & x<3")
    r should matchPattern { case parser.Success(_, _) => }
    val rule = r.get
    val variables = Map("x" -> 2.0)
    implicit val lookup = variables.apply _
    val clause = rule.asClause
    val truth: Clause[Double] = clause.transform(lookup,_.toString)
    truth() should matchPattern {case Success(true) => }
  }
  it should """evaluate x>1 & x<3 as false""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.term, "x>1 & x<3")
    r should matchPattern { case parser.Success(_, _) => }
    val rule = r.get
    val variables = Map("x" -> 4.0)
    implicit val lookup = variables.apply _
    val clause = rule.asClause
    val truth: Clause[Double] = clause.transform(lookup,_.toString)
    truth() should matchPattern {case Success(false) => }
  }
  "rule" should """parse x>1 & (x<3 | x=99) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.rule, "x>1 & (x<3 | x=99)")
    r should matchPattern { case parser.Success(_, _) => }
    val expected = Disjunction(List(Conjunction(List(Condition(Variable("x"),PredicateExpr(">",Number("1","1"))), Parentheses(Disjunction(List(Conjunction(List(Condition(Variable("x"),PredicateExpr("<",Number("3","1"))))), Conjunction(List(Condition(Variable("x"),PredicateExpr("=",Number("99","1"))))))))))))
    r.get should matchPattern { case `expected` => }
  }
}
