package com.phasmid.laScala

package truth

trait Truth extends (()=>Boolean) { self =>
  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluates if p evaluates as false
    *
    * @param t the other Predicate
    * @return a Predicate which is the disjunctive (and) combination of p with self
    */
  def &:(t: Truth): Truth = And(t, self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluates if p evaluates as true
    *
    * @param t the other Predicate
    * @return a Predicate which is the conjunctive (or) combination of p with self
    */
  def |:(t: Truth): Truth = Or(t, self)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param t the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the disjunctive (and) combination of self with p
    */
  def :&(t: Truth): Truth = And(self, t)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param t the other Predicate (p will not be evaluated if self evaluates as true)
    * @return a Predicate which is the conjunctive (or) combination of self with p
    */
  def :|(t: Truth): Truth = Or(self, t)

}
case class And(p1: Truth, p2: Truth) extends Truth {
  def apply(): Boolean = p1() && p2()
}

case class Or(p1: Truth, p2: Truth) extends Truth {
  def apply(): Boolean = p1() || p2()
}

case class BoundPredicate[T](x: T, predicate: Predicate[T]) extends Truth {
  def apply(): Boolean = predicate(x)
}

case object True extends Truth {
  def apply(): Boolean = true
}

case object False extends Truth {
  def apply(): Boolean = false
}
