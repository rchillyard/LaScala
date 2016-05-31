package com.phasmid.laScala.parser

import com.phasmid.laScala.truth.{BoundPredicate, True, Truth}

import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by scalaprof on 5/30/16.
  */
class RuleParser extends JavaTokenParsers {

  val booleanOp = regex(""">|>=|<|<=|=|!=""".r)
  val lesserOp = regex("""<|<=""".r)
  val identifier = regex("""\w+""".r)
  val identifierWithPeriods = regex("""[\w\.]+""".r)

  def rule: Parser[Rule] = repsep(term, "|") ^^ { case ts => Disjunction(ts) }

  def term: Parser[Rule] = repsep(factor, "&" ) ^^ { case fs => Conjunction(fs) }

  def factor: Parser[Rule] = (clause | "(" ~> rule <~ ")" | failure("problem with factor")) ^^ { case c: Condition => c; case r: Rule => Parentheses(r) }

  def clause: Parser[Rule] = identifier ~ predicate ^^ { case s ~ p => Condition(Variable(s),p)}

  def predicate: Parser[PredicateExpr] = booleanOp ~ value ^^ { case o ~ v => PredicateExpr(o,v) }

  // TODO why can't we pass Option[String] into Number and simply match on n ~ o?
  def value: Parser[Value] = (number ~ opt(suffix) | lookup | failure("problem with value")) ^^ { case s: String => Variable(s); case n ~ Some(x) => Number(n.toString,x.toString); case n ~ None => Number(n.toString,"1")}

  def number: Parser[String] = floatingPointNumber | wholeNumber  | failure("problem with number")

  def suffix: Parser[String] = ("""[BMK%]""".r | """@""".r ~ identifier | failure("problem with suffix")) ^^ { case at ~ id => id.toString; case s => s.toString; }

  def lookup: Parser[String] = ("""${""" ~ identifierWithPeriods <~ """}""" | "$" ~ identifier) ^^ { case _ ~ x => x }
}

sealed trait Rule {
  def toTruth(implicit m: String=>Double): Truth
}

case class Conjunction(fs: List[Rule]) extends Rule {
  def toTruth(implicit m: String=>Double): Truth = fs.foldLeft[Truth](True)((a, b) => a :& b.toTruth)
}

case class Disjunction(ts: List[Rule]) extends Rule {
  def toTruth(implicit m: String=>Double): Truth = ts.foldLeft[Truth](True)((a, b) => a :| b.toTruth)
}

case class Parentheses(rule: Rule) extends Rule {
  def toTruth(implicit m: String=>Double): Truth = rule.toTruth
}

case class Condition(subject: Variable, predicate: PredicateExpr) extends Rule {
  import Variable._
  def toTruth(implicit m: String=>Double): Truth = new BoundPredicate[Double](subject,predicate)
}

sealed trait Value

case class PredicateExpr(operator: String, operands: Value)

case class Number(s: String, m: String) extends Value

case class Variable(s: String) extends Value

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
