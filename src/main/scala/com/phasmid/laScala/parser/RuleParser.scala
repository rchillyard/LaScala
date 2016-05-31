package com.phasmid.laScala.parser

import com.phasmid.laScala.clause.{Clause, Truth}

import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * RuleParser is a parser-combinator which parses rules. [Wow, that's a surprise!]
  * A Rule is defined as a sealed trait which is extended by several different case classes, each defining
  * a different method of combining rules together into a tree of rules.
  * In general, a leaf rule is a Condition and is made up of two features: a subject and a predicate.
  *
  * It's important to note that all aspects of these rules are String-based. There is absolutely no conversion to numeric
  * types anywhere in this class.
  *
  * The trait Rule has only one method: asClause which returns a Clause[String]
  *
  * Created by scalaprof on 5/30/16.
  */
class RuleParser extends JavaTokenParsers {

  def rule: Parser[Rule] = repsep(term, "|") ^^ { case ts => Disjunction(ts) }

  def term: Parser[Rule] = repsep(factor, "&" ) ^^ { case fs => Conjunction(fs) }

  def factor: Parser[Rule] = (condition | "(" ~> rule <~ ")" | failure("problem with factor")) ^^ { case c: Condition => c; case r: Rule => Parentheses(r) }

  def condition: Parser[Rule] = identifier ~ predicate ^^ { case s ~ p => Condition(Variable(s),p)}

  def predicate: Parser[PredicateExpr] = booleanOp ~ value ^^ { case o ~ v => PredicateExpr(o,v) }

  // TODO why can't we pass Option[String] into Number and simply match on n ~ o?
  def value: Parser[Value] = (number ~ opt(suffix) | lookup | failure("problem with value")) ^^ { case s: String => Variable(s); case n ~ Some(x) => Number(n.toString,x.toString); case n ~ None => Number(n.toString,"1")}

  def number: Parser[String] = floatingPointNumber | wholeNumber  | failure("problem with number")

  def suffix: Parser[String] = ("""[BMK%]""".r | """@""".r ~ identifier | failure("problem with suffix")) ^^ { case at ~ id => id.toString; case s => s.toString; }

  def lookup: Parser[String] = ("""${""" ~ identifierWithPeriods <~ """}""" | "$" ~ identifier) ^^ { case _ ~ x => x }

  val booleanOp = regex(""">|>=|<|<=|=|!=""".r)
  val lesserOp = regex("""<|<=""".r)
  val identifier = regex("""\w+""".r)
  val identifierWithPeriods = regex("""[\w\.]+""".r)
}

sealed trait Rule {
  def asClause: Clause[String]
}

case class Conjunction(fs: List[Rule]) extends Rule {
  def asClause: Clause[String] = fs.foldLeft[Clause[String]](Truth(true))((a, b) => a :& b.asClause)
}

case class Disjunction(ts: List[Rule]) extends Rule {
  def asClause: Clause[String] = ts.foldLeft[Clause[String]](Truth(false))((a, b) => a :| b.asClause)
}

case class Parentheses(rule: Rule) extends Rule {
  def asClause: Clause[String] = rule.asClause
}

case class Condition(subject: Variable, predicate: PredicateExpr) extends Rule {
  def asClause: Clause[String] = new com.phasmid.laScala.clause.BoundPredicate[String](subject.toString,predicate)
}

sealed trait Value

/**
  * a predicate expression consisting of an operator and an operand.
  *
  * CONSIDER making the second parameter a Seq[Value]
  *
  * @param operator the operator. for example "<", ">=", etc.
  * @param operand the operand, i.e. the RHS of the operator.
  */
case class PredicateExpr(operator: String, operand: Value)

/**
  * a Number with a suffix multiplier
  * @param s the number in string form
  * @param m a multiplier such as B, M, K or 1 for multiples of 1000000000, 1000000, 1000, or 1 respectively.
  */
case class Number(s: String, m: String) extends Value

/**
  * a Variable reference.
  * @param s the key to the variable.
  */
case class Variable(s: String) extends Value {
  override def toString = s
}

class RuleException(s: String) extends Exception(s"rule problem: $s")

object Variable {
  implicit def convertToValue(x: Variable)(implicit m: String=>Double): Double = m(x.s)
//  implicit def convertToValue(x: Variable)(implicit m: String=>Int): Int = m(x.s)
}
object Number {
  implicit def convertToDouble(x: Number): Double = x match {
    case Number(i,f) => i.toDouble * getDoubleFactor(f)
  }
  implicit def convertToInteger(x: Number): Int = x match {
    case Number(i,f) => i.toInt * getIntFactor(f)
  }
  private def getIntFactor(f: String): Int = f match {
    case "B" => 1000 * getIntFactor("M")
    case "M" => 1000 * getIntFactor("K")
    case "K" => 1000 * getIntFactor("1")
    case "1" => 1
    case "%" => throw new RuleException("Number factor % not supported for Int")
    case _ => throw new RuleException("Number factor must be B, M, K, or 1")
  }
  private def getDoubleFactor(f: String): Double = f match {
    case "%" => 0.01
    case _ => getIntFactor(f)
  }
}
