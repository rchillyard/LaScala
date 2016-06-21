package com.phasmid.laScala.parser

import com.phasmid.laScala._
import com.phasmid.laScala.predicate.{Predicate, PredicateException}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Sealed trait RuleLike which describes the result of parsing a rule.
  * In this context, a rule is defined by several different case classes, each defining
  * a different method of combining rules together into a tree of rules.
  * In general, a leaf rule is a Condition and is made up of two features: a subject and a predicate.
  */
sealed trait RuleLike {
  /**
    * Method to convert this (parsed) RuleLike instance into a Rule[String]
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String]
}

case class Conjunction(fs: List[RuleLike]) extends RuleLike {
  /**
    * Method to convert this Conjunction into a conjunction of Rules
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String] = fs match {
    case Nil => InvalidRule(new PredicateException("empty Conjunction"))
    case h :: Nil => h.asRule
    case _ => fs.foldLeft[Rule[String]](Truth(true))((a, b) => a :& b.asRule)
  }
}

case class Disjunction(ts: List[RuleLike]) extends RuleLike {
  /**
    * Method to convert this Disjunction into a disjunction of Rules
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String] = ts match {
    case Nil => InvalidRule(new PredicateException("empty Disjunction"))
    case h :: Nil => h.asRule
    case _ => ts.foldLeft[Rule[String]](Truth(false))((a, b) => a :| b.asRule)
  }
}

case class Parentheses(rule: RuleLike) extends RuleLike {
  /**
    * Method to convert this parenthetical RuleLike into a Rule
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String] = rule.asRule
}

/**
  * Condition: defines a leaf RuleLike instance.
  *
  * @param subject   the subject of this condition
  * @param predicate the predicate of this condition
  */
case class Condition(subject: String, predicate: PredicateExpr) extends RuleLike {
  // NOTE that this involves an implicit conversion from PredicateExpr to Predicate
  val p: Predicate[String] = predicate match {
    case e: RangePredicateExpr => e
    case b: BooleanPredicateExpr => b
  }
  def asRule: Rule[String] = new BoundPredicate[String](subject, p)
}

/**
  * Truth value: always or never true
  *
  * @param b the truth value
  */
case class TruthValue(b: Boolean) extends RuleLike {
  def asRule: Rule[String] = Truth(b)
}

/**
  * TODO Trait Expression which needs a better description
  */
trait Expression {
  def toRPN: List[String]

  def asString: String
}

sealed trait PredicateExpr

/**
  * a predicate expression consisting of an operator and an operand.
  *
  * @param operator the operator. for example "<", ">=", etc.
  * @param operand  the operand, i.e. the RHS of the operator.
  */
case class BooleanPredicateExpr(operator: String, operand: Expression) extends PredicateExpr

/**
  * a predicate expression consisting of an two bounds
  *
  * @param operand1 the lower bound
  * @param operand2 the upper bound
  */
case class RangePredicateExpr(operand1: Expression, operand2: Expression) extends PredicateExpr

/**
  * RuleParser is a parser-combinator which parses rules. [Wow, that's a surprise!]
  * A RuleLike is defined as a sealed trait which is extended by several different case classes, each defining
  * a different method of combining rules together into a tree of rules.
  * In general, a leaf rule is a Condition and is made up of two features: a subject and a predicate.
  *
  * It's important to note that all aspects of these rules are String-based. There is absolutely no conversion to numeric
  * types anywhere in this class.
  *
  * The trait RuleLike has only one method: asRule which returns a Rule[String]
  *
  * Created by scalaprof on 5/30/16.
  */
class RuleParser extends JavaTokenParsers {

  def parseRule(s: String): Try[RuleLike] = {
    parseAll(rule, s) match {
      case this.Success(p, _) => scala.util.Success(p)
      case this.Failure(x, _) => scala.util.Failure(new PredicateException(s"parse failure on $s: $x"))
      case this.Error(x, _) => scala.util.Failure(new PredicateException(s"parse error on $s: $x"))
    }
  }

  def rule: Parser[RuleLike] = repsep(term, alternation) ^^ { case ts => Disjunction(ts) }

  def alternation: Parser[String] = "|" | "(?i)or".r

  def term: Parser[RuleLike] = repsep(factor, ampersand) ^^ { case fs => Conjunction(fs) }

  def ampersand: Parser[String] = "&" | "(?i)and".r

  def factor: Parser[RuleLike] = (always | never | condition | "(" ~> rule <~ ")" | failure("problem with factor")) ^^ {
    case t: TruthValue => t
    case c: Condition => c
    case r: RuleLike => Parentheses(r)
  }

  def always: Parser[RuleLike] = "(?i)always".r ^^ { case _ => TruthValue(true) }
  def never: Parser[RuleLike] = "(?i)never".r ^^ { case _ => TruthValue(false) }

  def condition: Parser[RuleLike] = identifier ~ predicate ^^ { case s ~ p => Condition(s, p) }

  def predicate: Parser[PredicateExpr] = booleanPredicate | rangePredicate
  def booleanPredicate: Parser[BooleanPredicateExpr] = booleanOp ~ expr ^^ { case o ~ v => BooleanPredicateExpr(o, v) }
  def rangePredicate: Parser[RangePredicateExpr] = "in" ~ expr ~ "..." ~ expr ^^ { case i ~ l ~ o ~ h => RangePredicateExpr(l, h) }

  val booleanOp = regex(""">|>=|<|<=|=|!=""".r)
  // TODO implement this...
  val lesserOp = regex("""<|<=""".r)
  val identifier = regex("""\w+""".r)
  val identifierWithPeriods = regex("""[\w\.]+""".r)

  abstract class ExprFactor extends Expression

  case class Expr(t: ExprTerm, ts: List[String ~ ExprTerm]) extends Expression {
    def toRPN = {
      val stack = new mutable.Stack[String]()
      def shunt(xs: List[String], et: String ~ ExprTerm): List[String] = et match {
        case op ~ x => stack.push(op); xs ++ x.toRPN
      }
      val rpn: List[String] = ts.foldLeft(t.toRPN)(shunt(_, _))
      rpn ++ stack.elems.reverse
    }

    def asString = ts.foldLeft(t.toString)(_ + _.toString)
  }

  case class ExprTerm(f: ExprFactor, fs: List[String ~ ExprFactor]) extends Expression {
    def toRPN = {
      val stack = new mutable.Stack[String]()
      def shunt(xs: List[String], et: String ~ ExprFactor): List[String] = et match {
        case op ~ x => stack.push(op); xs ++ x.toRPN
      }
      val rpn: List[String] = fs.foldLeft(f.toRPN)(shunt(_, _))
      rpn ++ stack.elems.reverse
    }

    def asString = fs.foldLeft(f.toString)(_ + _.toString)
  }

  /**
    * a Number with a suffix multiplier
    *
    * @param n the number in string form
    * @param m a multiplier such as B, M, K or 1 for multiples of 1000000000, 1000000, 1000, or 1 respectively.
    */
  case class Number(n: String, m: String) extends ExprFactor {
    def expand(s: String): List[String] = s match {
      case "B" => expand("K") ++ expand("M")
      case "M" => expand("K") ++ expand("K")
      case "K" => List("1000", "*")
      case "1" => List()
      case "%" => List("100", "/")
      case _ => throw new RuleException("Number exprFactor must be B, M, K, %, or 1")
    }

    def asString: String = s"$n*${expand(m)}"

    def toRPN: List[String] = n +: expand(m)

    //    override def toString = asString
  }

  case class Variable(x: String) extends ExprFactor {
    def asString: String = "$" + s"$x"

    def toRPN: List[String] = List(asString)
  }

  /**
    * XXX this appears not to be tested
    *
    * @param e the expression inside the parentheses
    */
  case class ExprParentheses(e: Expr) extends ExprFactor {
    def toRPN = e.toRPN

    override def asString: String = s"($e)"
  }

  /**
    * XXX this appears not to be tested
    *
    * @param s the String which defines this expression
    */
  case class ExprValue(s: String) extends Expression {
    def toRPN = List("$" + s)

    override def asString: String = s"($s)"
  }

  def expr: Parser[Expression] = exprTerm ~ rep("+" ~ exprTerm | "-" ~ exprTerm | failure("expr")) ^^ {
    case t ~ r => r match {
      case x: List[String ~ ExprTerm] => Expr(t, x)
    }
  }

  def exprTerm: Parser[ExprTerm] = exprFactor ~ rep("*" ~ exprFactor | "/" ~ exprFactor | failure("exprTerm")) ^^ {
    case f ~ r => r match {
      case x: List[String ~ Expression] => ExprTerm(f, x)
    }
  }

  def exprFactor: Parser[ExprFactor] = (number ~ opt(suffix) | lookup | "(" ~ expr ~ ")" | failure("exprFactor")) ^^ {
    case "(" ~ e ~ ")" => e match {
      case x: Expr => ExprParentheses(x)
    }
    // XXX why do we not see n and p as Strings?
    case n ~ Some(p) => Number(n.toString, p.toString)
    case n ~ None => Number(n.toString, "1")
    case s: String => Variable(s)
  }

  def number: Parser[String] = floatingPointNumber | wholeNumber | failure("problem with number")

  def suffix: Parser[String] = ("""[BMK%]""".r | """@""".r ~ identifier | failure("problem with suffix")) ^^ { case at ~ id => id.toString; case s => s.toString; }

  def lookup: Parser[String] = ("""${""" ~ identifierWithPeriods <~ """}""" | "$" ~ identifier) ^^ { case _ ~ x => x }
}



