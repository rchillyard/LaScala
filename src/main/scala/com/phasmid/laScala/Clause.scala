package com.phasmid.laScala

package clause

import scala.util.{Failure, Success, Try}
import com.phasmid.laScala.FP._
import com.phasmid.laScala.parser.Valuable
import com.phasmid.laScala.parser.rpn.RPN

/**
  * Trait Clause which extends ()=>Boolean. An alternative name might be TruthValue or just Truth.
  *
  * Thus the apply method for a Clause will always return true or false, with no input parameter.
  *
  * @tparam T
  */
sealed trait Clause[T] extends (()=>Try[Boolean]) { self =>

  /**
    * Method to transform this Clause[T] into a Clause[U].
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * @param lf T=>U function to be applied to the lhs of the clause (the variable)
    * @param rf T=>U function to be applied to the rhs of the clause (the predicate)
    * //@tparam U
    * @return a Clause[T] which is equivalent (truth-wise) to this Clause.
    */
  def transform[U : Ordering](lf: (T) => U, rf: (T) => U): Clause[U]

  /**
    * Method to transform this Clause[T] into a Clause[U].
    *
    * The primary purpose of this method is to allow a lookup to occur (the xf functions).
    * Typically, T will be String and U will be Double or Int.
    *
    * @param lf T=>U function to be applied to the lhs of the clause (the variable)
    * @param rf T=>U function to be applied to the rhs of the clause (the predicate)
    * //@tparam U
    * @return a Clause[T] which is equivalent (truth-wise) to this Clause.
    */
  def tryTransform[U : Ordering](lf: (T) => Try[U], rf: (T) => Try[U]): Try[Clause[U]]

  /**
    * Conjunctive combination of self with another Clause.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if c evaluates as false
    *
    * @param c the other Clause
    * @return a Clause which is the conjunctive (and) combination of c with self
    */
  def &:(c: => Clause[T]): Clause[T] = new And(c, self)

  /**
    * Disjunctive combination of self with another Clause.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if c evaluates as true
    *
    * @param c the other Clause
    * @return a Clause which is the disjunctive (or) combination of c with self
    */
  def |:(c: => Clause[T]): Clause[T] = new Or(c, self)

  /**
    * Conjunctive combination of self with another Clause.
    *
    * Associates to the left.
    *
    * @param c the other Clause (c will not be evaluated if self evaluates as false)
    * @return a Clause which is the conjunctive (and) combination of self with c
    */
  def :&(c: => Clause[T]): Clause[T] = new And(self, c)

  /**
    * Disjunctive combination of self with another Clause.
    *
    * Associates to the left.
    *
    * @param c the other Clause (c will not be evaluated if self evaluates as true)
    * @return a Clause which is the disjunctive (or) combination of self with c
    */
  def :|(c: => Clause[T]): Clause[T] = new Or(self, c)
}

abstract class BaseClause[T](name: String) extends Clause[T] {
  override def toString = name
}

/**
  * "And" sub-class of Clause giving the conjunction of c1 and c2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param c1 the clause which is always evaluated
  * @param c2 the clause which may not be evaluated
  * //@tparam T
  */
class And[T](c1: Clause[T], c2: => Clause[T]) extends BaseClause[T](s"$c1 & $c2") {
  def transform[U : Ordering](f: (T) => U, g: (T) => U): Clause[U] = tryTransform(Clause.lift(f),Clause.lift(g)).get
  def tryTransform[U : Ordering](f: (T) => Try[U], g: (T) => Try[U]): Try[Clause[U]] = for (x <- c1 tryTransform (f, g); y <- c2 tryTransform (f, g)) yield new And(x,y)
  def apply(): Try[Boolean] = map2(c1(),c2())(_&&_)
}

/**
  * "Or" sub-class of Clause giving the disjunction of c1 and c2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param c1 the clause which is always evaluated
  * @param c2 the clause which may not be evaluated
  * //@tparam T
  */
class Or[T](c1: Clause[T], c2: => Clause[T]) extends BaseClause[T](s"($c1 | $c2)") {
  override def transform[U : Ordering](f: (T) => U, g: (T) => U): Clause[U] = new Or[U](c1 transform (f, g), c2 transform (f, g))
  def tryTransform[U : Ordering](f: (T) => Try[U], g: (T) => Try[U]): Try[Clause[U]] = for (x <- c1 tryTransform (f, g); y <- c2 tryTransform (f, g)) yield new Or(x,y)
  def apply(): Try[Boolean] = map2(c1(),c2())(_||_)
}

/**
  * Bound predicate sub-class of Clause giving the result of applying a parameter to a predicate.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param t the parameter
  * @param p the Predicate
  * //@tparam T
  */
class BoundPredicate[T](t: => T, p: => Predicate[T]) extends BaseClause[T](s"($t $p)") { self =>
  println(s"created new BoundPredicate with t=$t and p=$p")
  override def transform[U : Ordering](f: (T) => U, g: (T) => U): Clause[U] = {
    println(s"transform BoundPredicate $self with $f and $g")
    new BoundPredicate[U](f(t), p map g)
  }
  def tryTransform[U : Ordering](f: (T) => Try[U], g: (T) => Try[U]): Try[Clause[U]] = ???
//  {
//    val pDash: Predicate[Try[U]] = p map g
//    for (u <- f(t); y <- p map g; z <- y) yield new BoundPredicate[U](u, z)
//  }
  def apply(): Try[Boolean] = p(t)
}

/**
  * This sub-class of Clause simply yields a fixed boolean expression
  *
  * @param b the fixed boolean expression
  * //@tparam T
  */
case class Truth[T](b: Boolean) extends BaseClause[T](s"$b") {
  def transform[U : Ordering](f: (T) => U, g: (T) => U): Clause[U] = Truth(b).asInstanceOf[Clause[U]]
  def tryTransform[U : Ordering](f: (T) => Try[U], g: (T) => Try[U]): Try[Clause[U]] = Success(Truth(b).asInstanceOf[Clause[U]])
  def apply(): Try[Boolean] = Success(b)
}

case class InvalidClause[T](x: Throwable)  extends BaseClause[T](s"invalid: $x") {
  override def transform[U : Ordering](f: (T) => U, g: (T) => U): Clause[U] = InvalidClause(x).asInstanceOf[Clause[U]]
  def tryTransform[U : Ordering](f: (T) => Try[U], g: (T) => Try[U]): Try[Clause[U]] = Success(InvalidClause(x).asInstanceOf[Clause[U]])
  def apply(): Try[Boolean] = Failure(x)
}

object Clause {
  def convertFromStringClauseToOrderingClause[X : Valuable](c: Clause[String], variables: Map[String,X]): Clause[X] = {
    val stringToInt: (String) => X = variables.apply _
    implicit val lookup: (String) => Option[X] = variables.get _
    // CONSIDER rewriting this (and a lot of other stuff--not easy) so that we don't have to use get
    val evaluateExpression: (String) => X = {s => RPN.evaluate[X](s).get}
    c transform(stringToInt,evaluateExpression)
  }
  def lift[T,U](f: T=>U): T=>Try[U] = {t=>Try(f(t))}
}