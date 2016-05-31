package com.phasmid.laScala

package clause

import scala.util.{Success, Try, Failure}
import com.phasmid.laScala.FP._

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
    * The primary purpose of this method is to allow a lookup to occur (the f function).
    * Typically, T will be String and U will be Double or Int.
    * In such a case, it is usually OK to make g simply "_.toString"
    *
    * @param f T=>U function
    * @param g U=>T function (i.e. inverse function)
    * //@tparam U
    * @return a Clause[T] which is equivalent (truth-wise) to this Clause.
    */
  def transform[U](f: T=>U, g: U=>T): Clause[U]

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

/**
  * "And" sub-class of Clause giving the conjunction of c1 and c2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param c1 the clause which is always evaluated
  * @param c2 the clause which may not be evaluated
  * //@tparam T
  */
class And[T](c1: Clause[T], c2: => Clause[T]) extends Clause[T] {
  override def transform[U](f: (T) => U, g: U=>T): Clause[U] = new And[U](c1 transform (f,g), c2 transform (f,g))
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
class Or[T](c1: Clause[T], c2: => Clause[T]) extends Clause[T] {
  override def transform[U](f: (T) => U, g: U=>T): Clause[U] = new Or[U](c1 transform (f,g), c2 transform (f,g))
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
class BoundPredicate[T](t: => T, p: => Predicate[T]) extends Clause[T] {
  override def transform[U](f: (T) => U, g: U=>T): Clause[U] = new BoundPredicate[U](f(t), p transform g)
  def apply(): Try[Boolean] = p(t)
}

/**
  * This sub-class of Clause simply yields a fixed boolean value
  *
  * @param b the fixed boolean value
  * //@tparam T
  */
case class Truth[T](b: Boolean) extends Clause[T] {
  override def transform[U](f: (T) => U, g: (U) => T): Clause[U] = Truth(b).asInstanceOf[Clause[U]]
  def apply(): Try[Boolean] = Success(b)
}
