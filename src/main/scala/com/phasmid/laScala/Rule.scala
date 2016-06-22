package com.phasmid.laScala

import com.phasmid.laScala.FP._
import com.phasmid.laScala.parser.Valuable
import com.phasmid.laScala.parser.rpn.RPN
import com.phasmid.laScala.predicate.Predicate

import scala.util.{Failure, Success, Try}

/**
  * Trait Rule which extends ()=>Boolean. An alternative name might be TruthValue or just Truth.
  *
  * Thus the apply method for a Rule will always return true or false, with no input parameter.
  *
  * A (leaf) rule is essentially an expression of the form: variable predicate
  * where predicate is an operator followed by an expression.
  *
  * A rule can be represented for any type T, including String.
  * However, if you want to be able to evaluate the truth value of a rule, then either
  * (1) T extends Valuable, or
  * (2) an implicit map from String to T is made available
  *
  * //  * @tparam T
  */
sealed trait Rule[T] extends (() => Try[Boolean]) {
  self =>

  /**
    * Method to transform this Rule[T] into a Rule[U].
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * @param lf T=>U function to be applied to the lhs of the rule (the variable)
    * @param rf T=>U function to be applied to the rhs of the rule (the predicate)
    *           //@tparam U
    * @return a Rule[T] which is equivalent (truth-wise) to this Rule.
    */
  def transform[U: Ordering](lf: (T) => U, rf: (T) => U): Rule[U]

  /**
    * Conjunctive combination of self with another Rule.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if c evaluates as false
    *
    * @param r the other Rule
    * @return a Rule which is the conjunctive (and) combination of r with self
    */
  def &:(r: => Rule[T]): Rule[T] = new And(r, self)

  /**
    * Disjunctive combination of self with another Rule.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if r evaluates as true
    *
    * @param r the other Rule
    * @return a Rule which is the disjunctive (or) combination of r with self
    */
  def |:(r: => Rule[T]): Rule[T] = new Or(r, self)

  /**
    * Conjunctive combination of self with another Rule.
    *
    * Associates to the left.
    *
    * @param r the other Rule (r will not be evaluated if self evaluates as false)
    * @return a Rule which is the conjunctive (and) combination of self with r
    */
  def :&(r: => Rule[T]): Rule[T] = new And(self, r)

  /**
    * Disjunctive combination of self with another Rule.
    *
    * Associates to the left.
    *
    * @param r the other Rule (r will not be evaluated if self evaluates as true)
    * @return a Rule which is the disjunctive (or) combination of self with r
    */
  def :|(r: => Rule[T]): Rule[T] = new Or(self, r)
}

abstract class BaseRule[T](name: String) extends Rule[T] {
  override def toString = name
}

/**
  * "And" sub-class of Rule giving the conjunction of r1 and r2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param r1 the rule which is always evaluated
  * @param r2 the rule which may not be evaluated
  *           //@tparam T
  */
class And[T](r1: Rule[T], r2: => Rule[T]) extends BaseRule[T](s"$r1 & $r2") {
  override def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = new And[U](r1 transform(f, g), r2 transform(f, g))

  def apply(): Try[Boolean] = map2(r1(), r2())(_ && _)
}

/**
  * "Or" sub-class of Rule giving the disjunction of r1 and r2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param r1 the rule which is always evaluated
  * @param r2 the rule which may not be evaluated
  *           //@tparam T
  */
class Or[T](r1: Rule[T], r2: => Rule[T]) extends BaseRule[T](s"($r1 | $r2)") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = new Or[U](r1 transform(f, g), r2 transform(f, g))

  def apply(): Try[Boolean] = map2(r1(), r2())(_ || _)
}

/**
  * Bound predicate sub-class of Rule giving the result of applying a parameter to a predicate.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param t the parameter
  * @param p the Predicate
  *          //@tparam T
  */
class BoundPredicate[T](t: => T, p: => Predicate[T]) extends BaseRule[T](s"($t $p)") {
  self =>
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = new BoundPredicate[U](f(t), p map g)

  def apply(): Try[Boolean] = p(t)
}

/**
  * This sub-class of Rule simply yields a fixed boolean expression
  *
  * @param b the fixed boolean expression
  *          //@tparam T
  */
case class Truth[T](b: Boolean) extends BaseRule[T](s"$b") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = Truth(b).asInstanceOf[Rule[U]]

  def apply(): Try[Boolean] = Success(b)
}

case class InvalidRule[T](x: Throwable) extends BaseRule[T](s"invalid: $x") {
  def transform[U: Ordering](f: (T) => U, g: (T) => U): Rule[U] = InvalidRule(x).asInstanceOf[Rule[U]]

  def apply(): Try[Boolean] = Failure(x)
}

object Rule {
  def convertFromStringRuleToValuableRule[X: Valuable](r: Rule[String], lookup: String=>Option[X]): Rule[X] = {
    val stringToInt: (String) => X = { s => lookup(s).get}
    implicit val f = lookup
    // CONSIDER rewriting this (and a lot of other stuff--not easy) so that we don't have to use get
    val evaluateExpression: (String) => X = { s => RPN.evaluate[X](s).get }
    r transform(stringToInt, evaluateExpression)
  }

  def lift[T, U](f: T => U): T => Try[U] = { t => Try(f(t)) }
}

class RuleException(s: String) extends Exception(s"rule problem: $s")
