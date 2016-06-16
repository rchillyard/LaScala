package com.phasmid.laScala.parser

import com.phasmid.laScala.{FP, PredicateException}
import com.phasmid.laScala.clause.{Clause, InvalidClause, Truth}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try
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

  def parseRule(s: String): Try[Rule] = {
    parseAll(rule, s) match {
      case this.Success(p, _) => scala.util.Success(p)
      case this.Failure(x, _) => scala.util.Failure(new PredicateException(s"parse failure on $s: $x"))
      case this.Error(x, _) => scala.util.Failure(new PredicateException(s"parse error on $s: $x"))
    }
  }

  def rule: Parser[Rule] = repsep(term, "|") ^^ { case ts => Disjunction(ts) }

  def term: Parser[Rule] = repsep(factor, "&" ) ^^ { case fs => Conjunction(fs) }

  def factor: Parser[Rule] = (condition | "(" ~> rule <~ ")" | failure("problem with factor")) ^^ { case c: Condition => c; case r: Rule => Parentheses(r) }

  def condition: Parser[Rule] = identifier ~ predicate ^^ { case s ~ p => Condition(s,p)}

  def predicate: Parser[PredicateExpr] = booleanOp ~ expr ^^ { case o ~ v => PredicateExpr(o,v) }

  // TODO why can't we pass Option[String] into Number and simply match on n ~ o?
//  def expression: Parser[Expression] = (number ~ opt(suffix) | lookup | failure("problem with expression")) ^^ { case s: String => Variable(s); case n ~ Some(x) => Number(n.toString,x.toString); case n ~ None => Number(n.toString,"1")}


  val booleanOp = regex(""">|>=|<|<=|=|!=""".r)
  // TODO implement this...
  val lesserOp = regex("""<|<=""".r)
  val identifier = regex("""\w+""".r)
  val identifierWithPeriods = regex("""[\w\.]+""".r)

  abstract class ExprFactor extends Expression

  case class Expr(t: ExprTerm, ts: List[String ~ ExprTerm]) extends Expression {
    def toRPN = {
      val stack = new mutable.Stack[String] ()
      def shunt(xs: List[String], et: String ~ ExprTerm): List[String] = et match {
        case op ~ x => stack.push (op); xs ++ x.toRPN
      }
      val rpn: List[String] = ts.foldLeft (t.toRPN)(shunt (_, _) )
      rpn ++ stack.elems.reverse
    }
    def asString = ts.foldLeft(t.toString)(_+_.toString)
//    override def toString = asString
  }

  case class ExprTerm(f: ExprFactor, fs: List[String ~ ExprFactor]) extends Expression {
    def toRPN = {
      val stack = new mutable.Stack[String] ()
      def shunt(xs: List[String], et: String ~ ExprFactor): List[String] = et match {
        case op ~ x => stack.push (op); xs ++ x.toRPN
      }
      val rpn: List[String] = fs.foldLeft (f.toRPN)(shunt (_, _) )
      rpn ++ stack.elems.reverse
    }
    def asString = fs.foldLeft(f.toString)(_+_.toString)
//    override def toString = asString
  }
  /**
    * a Number with a suffix multiplier
    *
    * @param n the number in string form
    * @param m a multiplier such as B, M, K or 1 for multiples of 1000000000, 1000000, 1000, or 1 respectively.
    */
  case class Number(n: String, m: String) extends ExprFactor {
    def expand(s: String): List[String] = s match {
      case "B" => expand("K")++expand("M")
      case "M" => expand("K")++expand("K")
      case "K" => List("1000","*")
      case "1" => List()
      case "%" => List("100","/")
      case _ => throw new RuleException("Number exprFactor must be B, M, K, %, or 1")
    }
    def asString: String = s"$n*${expand(m)}"
    def toRPN: List[String] = n +: expand(m)
//    override def toString = asString
  }

  case class Variable(x: String) extends ExprFactor {
    def asString: String = "$"+s"$x"
    def toRPN: List[String] = List(asString)
//    override def toString = asString
  }



  //  case class Number(x: String, suffix: String = "1") extends Expression {
//    val suffixList = (suffix match {
//      case "1" => List[String]()
//        // TODO support the other suffixes
//      case "K" => List[String]("1000","*")
//    }
//      )
//    def toRPN = x +: suffixList
//    override def asString: String = x
//  }

  case class ExprParentheses(e: Expr) extends ExprFactor {
    def toRPN = e.toRPN
    override def asString: String = s"($e)"
//    override def toString = asString
  }

  case class ExprValue(s: String) extends Expression {
    def toRPN = List("$"+s)
    override def asString: String = s"($s)"
//    override def toString = asString
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
    case n ~ Some(p) => Number(n.toString,p.toString)
    case n ~ None => Number(n.toString,"1")
    case s: String => Variable(s)
  }

  def number: Parser[String] = floatingPointNumber | wholeNumber  | failure("problem with number")

  def suffix: Parser[String] = ("""[BMK%]""".r | """@""".r ~ identifier | failure("problem with suffix")) ^^ { case at ~ id => id.toString; case s => s.toString; }

  def lookup: Parser[String] = ("""${""" ~ identifierWithPeriods <~ """}""" | "$" ~ identifier) ^^ { case _ ~ x => x }

}

/**
  *
  */
trait Expression {
  def toRPN: List[String]
  def asString: String
}


sealed trait Rule {
  def asClause: Clause[String]
}

case class Conjunction(fs: List[Rule]) extends Rule {
  def asClause: Clause[String] = fs match {
    case Nil => InvalidClause(new PredicateException("empty Conjunction"))
    case h :: Nil => h.asClause
    case _ => fs.foldLeft[Clause[String]](Truth(true))((a, b) => a :& b.asClause)
  }
}

case class Disjunction(ts: List[Rule]) extends Rule {
  def asClause: Clause[String] = ts match {
    case Nil => InvalidClause(new PredicateException("empty Disjunction"))
    case h :: Nil => h.asClause
    case _ => ts.foldLeft[Clause[String]](Truth(false))((a, b) => a :| b.asClause)
  }
}

case class Parentheses(rule: Rule) extends Rule {
  def asClause: Clause[String] = rule.asClause
}

case class Condition(subject: String, predicate: PredicateExpr) extends Rule {
  def asClause: Clause[String] = new com.phasmid.laScala.clause.BoundPredicate[String](subject,predicate)
}


/**
  * a predicate expression consisting of an operator and an operand.
  *
  * CONSIDER making the second parameter a Seq[Value]
  *
  * @param operator the operator. for example "<", ">=", etc.
  * @param operand the operand, i.e. the RHS of the operator.
  */
case class PredicateExpr(operator: String, operand: Expression)

//case class Expression(s: String) extends Value {
//  override def asString = s
//}
class RuleException(s: String) extends Exception(s"rule problem: $s")

