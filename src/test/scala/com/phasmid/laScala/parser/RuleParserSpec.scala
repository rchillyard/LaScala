package com.phasmid.laScala.parser

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author scalaprof
 */
class RuleParserSpec extends FlatSpec with Matchers {
  "expression" should """parse 1 as Number("1","1")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()) => }
  }
  it should """parse 1.0K as Number("1.0","K")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1.0","K"),List()),List()) => }
  }
  it should """parse 1.0@m as Number("1.0","m")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0@m")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1.0","m"),List()),List()) => }
  }
  it should """parse $x as Variable(x)""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "$x")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Variable("x"),List()),List()) => }
  }
  it should """parse ${x.y} as Variable(x.y)""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "${x.y}")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Variable("x.y"),List()),List()) => }
  }
  it should """evaluate 1 as 1""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()) =>
      case _ => fail(r.get.toString)
    }
  }
  it should """evaluate 1.0K as 1000.0""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case parser.Expr(parser.ExprTerm(parser.Number("1.0","K"),List()),List()) =>
      case _ => fail(s"${r.get}")
    }
  }
//  "predicate" should """parse >1.0K as appropriate""" in {
//    val parser = new RuleParser()
//    val r = parser.parseAll(parser.predicate, ">1.0K")
//    r should matchPattern { case parser.Success(_, _) => }
//    r.get should matchPattern { case PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.ExprValue(parser.Number("1.0","K")),List()),List())) => }
//  }
//  "condition" should """parse x>1.0K as appropriate""" in {
//    val parser = new RuleParser()
//    val r = parser.parseAll(parser.condition, "x>1.0K")
//    r should matchPattern { case parser.Success(_, _) => }
//    r.get should matchPattern { case Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.ExprValue(parser.Number("1.0","K")),List()),List()))) => }
//  }
  // TODO remove this test (because it really belongs in RuleSpec)
//  it should """evaluate x>1.0K as true""" in {
//    val parser = new RuleParser()
//    val r = parser.parseAll(parser.condition, "x>1.0K")
//    r should matchPattern { case parser.Success(_, _) => }
//    println(r.get)
//    val variables = Map("x" -> 2000.0)
//    implicit val lookup = variables.apply _
//    val rule = r.get.asRule
//    println(rule)
//    val truth: Rule[Double] = rule.transform(FP.named("variables.apply _",lookup), FP.named("toDouble",_.toDouble))
//    println(truth)
//    truth() should matchPattern {case Success(true) => }
//  }
  "factor" should """parse x>1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.Number("1.0","K"),List()),List()))) => }
  }
  it should """parse (x>1.0K) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "(x>1.0K)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Parentheses(Disjunction(List(Conjunction(List(Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.Number("1.0","K"),List()),List())))))))) => }
  }
  it should """parse x>1 as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()))) => }
  }
  "term" should """parse x>1 & x<3 as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.term, "x>1 & x<3")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Conjunction(List(Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()))), Condition("x",PredicateExpr("<",parser.Expr(parser.ExprTerm(parser.Number("3","1"),List()),List()))))) => }
  }
  "rule" should """parse x>1 & (x<3 | x=99) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.rule, "x>1 & (x<3 | x=99)")
    r should matchPattern { case parser.Success(_, _) => }
    val expected = Disjunction(List(Conjunction(List(Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()))), Parentheses(Disjunction(List(Conjunction(List(Condition("x",PredicateExpr("<",parser.Expr(parser.ExprTerm(parser.Number("3","1"),List()),List()))))), Conjunction(List(Condition("x",PredicateExpr("=",parser.Expr(parser.ExprTerm(parser.Number("99","1"),List()),List()))))))))))))
    r.get should matchPattern { case `expected` => }
  }
  it should """parse x>1 & (x<3 | x=99) using parseRule""" in {
    val parser = new RuleParser()
    parser.parseRule("x>1 & (x<3 | x=99)") should matchPattern { case scala.util.Success(r) => }
  }
  "expression" should "parse 1 as 1" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()) => }
  }
  "exprFactor" should "parse 1 as 1" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.exprFactor, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Number("1","1") => }
  }
  "expr" should "parse 1 as 1" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List()) => }
    r.get.toRPN should matchPattern { case List("1") => }
  }
  it should "parse 1.0 as 1.0" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.toRPN should matchPattern { case List("1") => }
  }
  it should "parse (1+1) as 2.0" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1","1"),List()),List((parser.~("+",parser.ExprTerm(parser.Number("1","1"),List()))))) => }
    r.get.toRPN should matchPattern { case List("1","1","+") => }
  }
  it should "parse (1*2+1) as 3.0" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.toRPN should matchPattern { case List("1", "2", "*", "1", "+") => }
  }
  it should "parse (1*2+1-1.5) as 1.5" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1*2+1-1.5")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.toRPN should matchPattern { case List("1", "2", "*", "1", "1.5", "+", "-") => }
  }
  it should "parse (1*2+1-3/2) as 1.5" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.toRPN should matchPattern { case List("1", "2", "*", "1", "3", "2", "/", "+", "-") => }
  }
  it should "not parse (1*2+1-pi/2)" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1*2+1-pi/2")
    r should matchPattern { case parser.Failure("exprFactor", _) => }
  }
  it should "not parse (1?2)" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "(1?2)")
    r should matchPattern { case parser.Failure("`)' expected but `?' found", _) => }
  }
  it should "not parse (" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "(")
    r should matchPattern { case parser.Failure("exprFactor", _) => }
  }
  "x > $z" should "parse correctly" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.condition, "x > $z")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x",PredicateExpr(">",parser.Expr(parser.ExprTerm(parser.Variable("z"),List()),List()))) => }
  }
}
