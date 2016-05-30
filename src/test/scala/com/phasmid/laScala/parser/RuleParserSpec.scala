package com.phasmid.laScala.parser

import org.scalatest.{FlatSpec, Matchers}

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
    }
  }
  it should """evaluate 1.0K as 1000.0""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.value, "1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get match {
      case n: Number => val x: Double = n; x === 1000.0 +- 0.001
    }
  }
  "predicate" should """parse >1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.predicate, ">1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Predicate(">",Number("1.0","K")) => }
  }
  "clause" should """parse x>1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.clause, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Condition("x",Predicate(">",Number("1.0","K"))) => }
  }
  "factor" should """parse x>1.0K as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "x>1.0K")
    r should matchPattern { case parser.Success(_, _) => }
    println(r)
    r.get should matchPattern { case Condition("x",Predicate(">",Number("1.0","K"))) => }
  }
  it should """parse (x>1.0K) as appropriate""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.factor, "(x>1.0K)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Parentheses(Disjunction(Conjunction(Condition("x",Predicate(">",Number("1.0","K"))),List()),List())) => }
  }

  //  it should "parse 1.0 as 1.0" in {
//    val parser = new RuleParser()
//    val r = parser.parseAll(parser.factor, "1.0")
//    r should matchPattern { case parser.Success(_, _) => }
//    r.get.value should matchPattern { case Success(1.0) => }
//  }
//  "evaluate" should "parse 1=1 as true" in {
//    new RuleParser().parseRule("1=1") should matchPattern { case Success(true) => }
//  }
//  it should "parse 1.0=1.0 as true" in {
//    new RuleParser().parseRule("1.0=1.0") should matchPattern { case Success(true) => }
//  }
//  it should "parse 1.0=1.0000001 as true" in {
//    new RuleParser().parseRule("1.0=1.0000001") should matchPattern { case Success(true) => }
//  }
//  it should "parse 1.0=1.000001 as false" in {
//    new RuleParser().parseRule("1.0=1.000001") should matchPattern { case Success(false) => }
//  }
//  it should "parse 1>0 as true" in {
//    new RuleParser().parseRule("1>0") should matchPattern { case Success(true) => }
//  }
//  it should "parse 1>0|1<0 as true" in {
//    new RuleParser().parseRule("1 > 0 | 1 < 0") should matchPattern { case Success(true) => }
//  }
//  it should "parse 1>0 & 1<0 as false" in {
//    new RuleParser().parseRule("1>0 & 1<0") should matchPattern { case Success(false) => }
//  }
//  it should "parse 1>0 & (1<0) as false" in {
//    new RuleParser().parseRule("1>0 & (1<0)") should matchPattern { case Success(false) => }
//  }
//  it should "parse Always as true" in {
//    new RuleParser().parseRule("Always") should matchPattern { case Success(true) => }
//  }
//  it should "parse Never as false" in {
//    new RuleParser().parseRule("Never") should matchPattern { case Success(false) => }
//  }
//  "inRange" should "parse 1 in 0 thru 10 as true" in {
//    val parser = new RuleParser()
//    val result = parser.parseAll(parser.inRange,"1 in 0 thru 10")
//    result should matchPattern {case parser.Success(_,_) => }
//    result.get.value should matchPattern { case Success(true) => }
//  }
//  it should "parse 1 in 2 thru 10 as false" in {
//    val parser = new RuleParser()
//    val result = parser.parseAll(parser.inRange,"1 in 2 thru10")
//    result should matchPattern {case parser.Success(_,_) => }
//    result.get.value should matchPattern { case Success(false) => }
//  }
//  "bounded" should "parse 0 < 1 < 10 as true" in {
//    val parser = new RuleParser()
//    val result = parser.parseAll(parser.bounded,"0 < 1 < 10")
//    result should matchPattern {case parser.Success(_,_) => }
//    result.get.value should matchPattern { case Success(true) => }
//  }
//  it should "parse 2 < 1 < 10 as false" in {
//    val parser = new RuleParser()
//    val result = parser.parseAll(parser.bounded,"2 < 1 < 10")
//    result should matchPattern {case parser.Success(_,_) => }
//    result.get.value should matchPattern { case Success(false) => }
//  }
//  it should "parse 0.0 < 1 < 10.0 as true" in {
//    val parser = new RuleParser()
//    val result = parser.parseAll(parser.bounded,"0.0 < 1 < 10.0")
//    result should matchPattern {case parser.Success(_,_) => }
//    result.get.value should matchPattern { case Success(true) => }
//  }
//  "(" should "fail" in {
//    val parser = new RuleParse
//    val r = parser.parseAll(parser.expr, "(")
//    r should matchPattern { case parser.Failure("factor", _) => }
//  }
//  "1+2=2" should "fail" in {
//    val parser = new RuleParse
//    val r = parser.parseAll(parser.expr, "1+2=2")
//    r should matchPattern { case parser.Failure("expr", _) => }
//  }
}
