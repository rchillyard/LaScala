package com.phasmid.laScala.parser.rpn

import com.phasmid.laScala.FP
import com.phasmid.laScala.parser.Valuable

import scala.util._

trait Token[X] {
//  def evaluate(params: X*): Try[X]
}
case class Constant[X : Valuable](f: () => Try[X]) extends Token[X] {
//  override def evaluate(params: X*): Try[X] = params.size match {
//    case 0 => for (y <- f()) yield implicitly[Valuable[X]].unit(y)
//    case _ => Failure(new Exception(s"RPN logic error for Constant: too many params: $params"))
//  }
}
case class Number[X : Valuable](n: String) extends Token[X] {
//  override def evaluate(params: X*): Try[X] = params.size match {
//    case 0 => implicitly[Valuable[X]].fromString (n)
//    case _ => Failure(new Exception(s"RPN logic error for Number: too many params: $params"))
//  }
}
case class Monadic[X : Valuable](f: X=>Try[X]) extends Token[X] {
//  override def evaluate(params: X*): Try[X] = params.size match {
//    case 1 => for (y <- f(params(0))) yield implicitly[Valuable[X]].unit(y)
//    case _ => Failure(new Exception(s"RPN logic error for Monadic: requires exactly one parameter but received: $params"))
//  }
}
case class Dyadic[X : Valuable](f: (X,X)=>Try[X]) extends Token[X] {
//  override def evaluate(params: X*): Try[X] = params.size match {
//    case 2 => for (y <- f(params(0),params(1))) yield implicitly[Valuable[X]].unit(y)
//    case _ => Failure(new Exception(s"RPN logic error for Monadic: requires exactly two parameters but received: $params"))
//  }
}
case class Invalid[X](t: Throwable) extends Token[X] {
//  override def evaluate(params: X*): Try[X] = Failure(t)
}
/**
  * Created by scalaprof on 6/13/16.
  */
case class RPN[X : Valuable](stack: List[Token[X]]) extends Token[X] {
  def push(t: Token[X]) = RPN(t::stack)
  def evaluate(params: X*): Try[X] = {
    val (xt,xts) = inner(stack)
    if (xts.isEmpty) xt
    else Failure[X](new Exception(s"logic error: remaining values: $xts"))
  }
  private def inner(xts: List[Token[X]]): (Try[X], List[Token[X]]) = xts match {
    case Nil =>
      (Failure(new Exception("token list is empty")), xts)
    case Number(s) :: r0 =>
      (implicitly[Valuable[X]].fromString(s), r0)
    case Constant(f) :: r0 =>
      (f(), r0)
    case Monadic(f) :: r0 =>
      val (xt, r1) = inner(r0)
      (for (x <- xt; z <- f(x)) yield z, r1)
    case Dyadic(f) :: r0 =>
      val (xt, r1) = inner(r0)
      val (yt, r2) = inner(r1)
      (for (x <- xt; y <- yt; z <- f(y,x)) yield z, r2)
    case Invalid(t) :: r0 =>
      (Failure(t), r0)
    case _ =>
      (Failure(new Exception(s"token list is invalid: $xts")), xts)
  }
}
object Token {
  implicit def defaultLookup[X](s: String): Option[X] = Map[String,X]().get(s)
  def apply[X : Valuable](s: String)(implicit lookup: String=>Option[X]): Token[X] = {
    val n: Valuable[X] = implicitly[Valuable[X]]
    val floatingR = """(-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?)""".r
    val lookupR = """\$(\w+)""".r
    s match {
        // TODO flesh this out
      case floatingR(x,_,_,_) => Number[X](x)
      case floatingR(x,_,_) => Number[X](x)
      case floatingR(x,_) => Number[X](x)
      case floatingR(x) => Number[X](x)
      case lookupR(x) => Constant[X](() => FP.optionToTry(lookup(x),new Exception(s"no value for lookup: $x")))
      case "+" => Dyadic[X](n.plus(_, _))
      case "-" => Dyadic[X](n.minus(_, _))
      case "*" => Dyadic[X](n.times(_, _))
      case "/" => Dyadic[X](n.div(_, _))
      case "chs" => Monadic[X](n.negate(_))
      case _ => Invalid(new Exception(s"invalid token: $s"))
    }
  }
}
object RPN {
  implicit def lookup[X](s: String): Option[X] = Map[String,X]().get(s)
  def apply[X : Valuable](): RPN[X] = apply(List[Token[X]]())
  def apply[X : Valuable](xs: Seq[String])(implicit lookup: String=>Option[X]): RPN[X] = xs.foldLeft(RPN[X]())((r,x) => r.push(Token[X](x)))
  def apply[X : Valuable](w: String)(implicit lookup: String=>Option[X]): RPN[X] = apply(w.split(" ").toSeq)
  def evaluate[X : Valuable](w: String)(implicit lookup: String=>Option[X]): Try[X] = apply(w).evaluate()
}