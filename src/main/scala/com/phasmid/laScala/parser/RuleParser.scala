package com.phasmid.laScala.parser

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by scalaprof on 5/30/16.
  */
class RuleParser extends JavaTokenParsers {

  val booleanOp = regex(""">|>=|<|<=|=|!=""".r)
  val lesserOp = regex("""<|<=""".r)
  val identifier = regex("""\w+""".r)
  val identifierWithPeriods = regex("""[\w\.]+""".r)

  def rule: Parser[Rule] = term ~ repsep(term, "|") ^^ { case f ~ r => Disjunction(f, r) }

  def term: Parser[Rule] = factor ~ repsep(factor, "&" ) ^^ { case f ~ r => Conjunction(f, r) }

  def factor: Parser[Rule] = (clause | "(" ~> rule <~ ")") ^^ { case c: Condition => c; case r: Rule => Parentheses(r) }

  def clause: Parser[Rule] = identifier ~ predicate ^^ { case s ~ p => Condition(s.toString,p)}

  def predicate: Parser[Predicate] = booleanOp ~ value ^^ { case o ~ v => Predicate(o,v) }

  // TODO why can't we pass Option[String] into Number and simply match on n ~ o?
  def value: Parser[Value] = (number ~ opt(suffix) | lookup) ^^ { case s: String => Variable(s); case n ~ Some(x) => Number(n.toString,x.toString); case n ~ None => Number(n.toString,"1")}

  def number: Parser[String] = floatingPointNumber | wholeNumber  | failure("problem with number")

  def suffix: Parser[String] = ("""[BMK%]""".r | """@""".r ~ identifier | failure("problem with suffix")) ^^ { case at ~ id => id.toString; case s => s.toString; }

  def lookup: Parser[String] = ("""${""" ~ identifierWithPeriods <~ """}""" | "$" ~ identifier) ^^ { case _ ~ x => x }
}

trait Rule

trait Value

case class Conjunction(rule: Rule, ts: List[Rule]) extends Rule

case class Disjunction(rule: Rule, ts: List[Rule]) extends Rule

case class Parentheses(rule: Rule) extends Rule

case class Condition(subject: String, predicate: Predicate) extends Rule

case class Predicate(operator: String, operands: Value)

case class Number(s: String, m: String) extends Value {
  //    def factor(m: String): Double = m match {
  //      case "B" => 1000 * factor("M");
  //      case "M" => 1000 * factor("K");
  //      case "K" => 1000 * factor("1");
  //      case "%" => 0.01;
  //      case "1" => 1;
  //      case _ => throw new RuleException(s"invalid factor: $m")
  //    }
  //    override def toString = s"$s$m"
}

case class Variable(s: String) extends Value

class RuleException(s: String) extends Exception(s"rule problem: $s")

object Number {
  def getIntFactor(f: String): Int = f match {
    case "B" => 1000 * getIntFactor("M")
    case "M" => 1000 * getIntFactor("K")
    case "K" => 1000 * getIntFactor("1")
    case "1" => 1
    case "%" => throw new RuleException("Number factor % not supported for Int")
    case _ => throw new RuleException("Number factor must be B, M, K, or 1")
  }
  def getDoubleFactor(f: String): Double = f match {
    case "%" => 0.01
    case _ => getIntFactor(f)
  }
  implicit def convertToInteger(n: Number): Int = n match {
    case Number(i,f) => i.toInt * getIntFactor(f)
  }
  implicit def convertToDouble(n: Number): Double = n match {
    case Number(i,f) => i.toDouble * getDoubleFactor(f)
  }
}
