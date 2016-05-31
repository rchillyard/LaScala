package com.phasmid.laScala

package truth

trait Truth extends (()=>Boolean) { self =>
  /**
    * Conjunctive combination of self with another Truth.
    *
    * Associates to the right.
    *
    * Self will not be evaluates if t evaluates as false
    *
    * @param t the other Truth
    * @return a Truth which is the conjunctive (and) combination of t with self
    */
  def &:(t: => Truth): Truth = new And(t, self)

  /**
    * Disjunctive combination of self with another Truth.
    *
    * Associates to the right.
    *
    * Self will not be evaluates if t evaluates as true
    *
    * @param t the other Truth
    * @return a Truth which is the disjunctive (or) combination of t with self
    */
  def |:(t: => Truth): Truth = new Or(t, self)

  /**
    * Conjunctive combination of self with another Truth.
    *
    * Associates to the left.
    *
    * @param t the other Truth (t) will not be evaluated if self evaluates as false
    * @return a Truth which is the conjunctive (and) combination of self with t
    */
  def :&(t: => Truth): Truth = new And(self, t)

  /**
    * Disjunctive combination of self with another Truth.
    *
    * Associates to the left.
    *
    * @param t the other Truth (t) will not be evaluated if self evaluates as true
    * @return a Truth which is the disjunctive (or) combination of self with t
    */
  def :|(t: => Truth): Truth = new Or(self, t)

}

/**
  * "And" sub-class of Truth giving the conjunction of t1 and t2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param t1 the truth value which is always evaluated
  * @param t2 the truth value which may not be evaluated
  */
class And(t1: Truth, t2: => Truth) extends Truth {
  def apply(): Boolean = t1() && t2()
}

/**
  * "Or" sub-class of Truth giving the disjunction of t1 and t2.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param t1 the truth value which is always evaluated
  * @param t2 the truth value which may not be evaluated
  */
class Or(t1: Truth, t2: => Truth) extends Truth {
  def apply(): Boolean = t1() || t2()
}

/**
  * Bound predicate sub-class of Truth giving the result of applying a parameter to a predicate.
  * Not a case class so that we can have a call-by-name parameter
  *
  * @param t the parameter
  * @param p the Predicate
  */
class BoundPredicate[T](t: => T, p: => Predicate[T]) extends Truth {
  def apply(): Boolean = p(t)
}

/**
  * "True" sub-class of Truth which evaluates to true
  *
  */
case object True extends Truth {
  def apply(): Boolean = true
}

/**
  * "False" sub-class of Truth which evaluates to false
  *
  */
case object False extends Truth {
  def apply(): Boolean = false
}
