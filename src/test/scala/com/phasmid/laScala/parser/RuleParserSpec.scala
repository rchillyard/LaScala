package com.phasmid.laScala.parser

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
  * @author scalaprof
  */
class RuleParserSpec extends FlatSpec with Matchers {
  "expression" should """parse 1 as Number("1","1")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()) => }
  }
  it should """parse 1.0K as Number("1.0","K")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1.0", "K"), List()), List()) => }
  }
  it should """parse 1.0@m as Number("1.0","m")""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0@m")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1.0", "m"), List()), List()) => }
  }
  it should """parse $x as Variable(x)""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "$x")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Variable("x"), List()), List()) => }
  }
  it should """parse ${x.y} as Variable(x.y)""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "${x.y}")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Variable("x.y"), List()), List()) => }
  }
  it should """evaluate 1 as 1""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()) =>
      case _ => fail(r.get.toString)
    }
  }
  it should """evaluate 1.0K as 1000.0""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case parser.Expr(parser.ExprTerm(parser.Number("1.0", "K"), List()), List()) =>
      case _ => fail(s"${r.get}")
    }
  }
  "predicate" should "parse \" > $z\" correctly" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.predicate, " > $z")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Variable("z"), List()), List())) => }
  }
  it should "parse \" in $y ... $z\" correctly" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.predicate, "in $y ... $z")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case RangePredicateExpr(parser.Expr(parser.ExprTerm(parser.Variable("y"), List()), List()), parser.Expr(parser.ExprTerm(parser.Variable("z"), List()), List())) => }
  }
  "factor" should """parse x>1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1.0", "K"), List()), List()))) => }
  }
  it should """parse (x>1.0K) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "(x>1.0K)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Parentheses(Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1.0", "K"), List()), List())))))))) => }
  }
  it should """parse x>1 as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()))) => }
  }
  "term" should """parse x>1 & x<3 as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.term, "x>1 & x<3")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Conjunction(List(Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()))), Condition("x", BooleanPredicateExpr("<", parser.Expr(parser.ExprTerm(parser.Number("3", "1"), List()), List()))))) => }
  }
  "rule" should """parse x>1 & (x<3 | x=99) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.rule, "x>1 & (x<3 | x=99)")
    r should matchPattern { case parser.Success(_, _) => }
    val expected = Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()))), Parentheses(Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr("<", parser.Expr(parser.ExprTerm(parser.Number("3", "1"), List()), List()))))), Conjunction(List(Condition("x", BooleanPredicateExpr("=", parser.Expr(parser.ExprTerm(parser.Number("99", "1"), List()), List()))))))))))))
    r.get should matchPattern { case `expected` => }
  }
  it should """parse x>1 and (x<3 or x=99) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.rule, "x>1 and (x<3 or x=99)")
    r should matchPattern { case parser.Success(_, _) => }
    val expected = Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()))), Parentheses(Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr("<", parser.Expr(parser.ExprTerm(parser.Number("3", "1"), List()), List()))))), Conjunction(List(Condition("x", BooleanPredicateExpr("=", parser.Expr(parser.ExprTerm(parser.Number("99", "1"), List()), List()))))))))))))
    r.get should matchPattern { case `expected` => }
  }
  it should """parse x>1 AND (x<3 OR x=99) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.rule, "x>1 AND (x<3 OR x=99)")
    r should matchPattern { case parser.Success(_, _) => }
    val expected = Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()))), Parentheses(Disjunction(List(Conjunction(List(Condition("x", BooleanPredicateExpr("<", parser.Expr(parser.ExprTerm(parser.Number("3", "1"), List()), List()))))), Conjunction(List(Condition("x", BooleanPredicateExpr("=", parser.Expr(parser.ExprTerm(parser.Number("99", "1"), List()), List()))))))))))))
    r.get should matchPattern { case `expected` => }
  }
  val expr = "x>1 & (x<3 | x=99)"
  "rule" should "parse " + expr + " using parseRule" in {
    val parser = new RuleParser()
    parser.parseRule(expr) should matchPattern { case scala.util.Success(_) => }
  }
  it should """parse "strikePrice > 95% * $basePrice & strikePrice < 105% * $basePrice & sharpeRatio > 0.4 & EV < $EBITDA*13"""" in {
    val parser = new RuleParser()
    parser.parseRule("strikePrice > 95% * $basePrice & strikePrice < 105% * $basePrice & sharpeRatio > 0.4 & EV < $EBITDA*13") should matchPattern { case scala.util.Success(_) => }
  }
  it should """parse "strikePrice in 95% * $basePrice...105% * $basePrice & sharpeRatio > 0.4 & EV < $EBITDA*13"""" in {
    val parser = new RuleParser()
    parser.parseRule("strikePrice in 95% * $basePrice...105% * $basePrice & sharpeRatio > 0.4 & EV < $EBITDA*13") should matchPattern { case scala.util.Success(_) => }
  }
  it should """parse "true & (strikePrice in 95% * $basePrice...105% * $basePrice & sharpeRatio > 0.4 & EV < $EBITDA*13)"""" in {
    val parser = new RuleParser()
    val rule = parser.parseRule("true & (strikePrice in 95% * $basePrice...105% * $basePrice & sharpeRatio > 0.4 & EV < $EBITDA*13)")
    rule should matchPattern { case scala.util.Success(_) => }
  }
  // FIXME this is string is the result of serializing the rule above
  ignore should """parse "true & (strikePrice in bounds 95 100 / $basePrice *..105 100 / $basePrice *) & (sharpeRatio >0.4) & (EV <$EBITDA 13 *)"""" in {
    val parser = new RuleParser()
    val rule = parser.parseRule("true & (strikePrice in bounds 95 100 / $basePrice *..105 100 / $basePrice *) & (sharpeRatio >0.4) & (EV <$EBITDA 13 *)")
    rule should matchPattern { case scala.util.Success(_) => }
  }
  "expression" should "parse 1 as 1" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()) => }
  }
  "exprFactor" should "parse 1 as 1" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.exprFactor, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Number("1", "1") => }
  }
  "quotedString" should """parse "Hello World!" correctly""" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.quotedString, """"Hello World!"""")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.asQuotedString should matchPattern { case Some("Hello World!") => }
  }
  "expr" should "parse 1 as 1" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List()) => }
    r.get.toRPN should matchPattern { case List("1") => }
  }
  it should """parse "Hello World!" correctly""" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.expr, """"Hello World!"""")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.asQuotedString should matchPattern { case Some("Hello World!") => }
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
    r.get should matchPattern { case parser.Expr(parser.ExprTerm(parser.Number("1", "1"), List()), List((parser.~("+", parser.ExprTerm(parser.Number("1", "1"), List()))))) => }
    r.get.toRPN should matchPattern { case List("1", "1", "+") => }
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
  "condition" should "parse \"x > $z\" correctly" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.condition, "x > $z")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x", BooleanPredicateExpr(">", parser.Expr(parser.ExprTerm(parser.Variable("z"), List()), List()))) => }
  }
  "truthValue" should "parse always" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.always, "always")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case TruthValue(true) => }
  }
  it should "parse Never" in {
    val parser = new RuleParser
    val r = parser.parseAll(parser.never, "Never")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case TruthValue(false) => }
  }
  "parseExpression" should """parse 1 as List("1")""" in {
    val parser = new RuleParser()
    val r = parser.parseExpression("1")
    r should matchPattern { case Success(_) => }
    r.get.toRPN shouldBe List("1")
  }
  it should "parse 1B as ..." in {
    val parser = new RuleParser()
    val et = parser.parseExpression("1B")
    et map {
      _.toRPN
    } should matchPattern { case Success(List("1", "1000", "*", "1000", "*", "1000", "*")) => }
  }
}
