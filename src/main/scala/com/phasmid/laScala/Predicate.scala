package com.phasmid.laScala

import com.phasmid.laScala.FP._

/**
  * This trait extends the function class T=>Boolean.
  * Predicates are intended to combine and form rules.
  * At present, a Predicate is a Functor but not a Monad.
  *
  * Created by robinhillyard on 5/24/16.
  */
trait Predicate[T] extends (T => Boolean) {
  self =>
  /**
    * The map function for Predicate.
    * Note that the parameter f is an S=>T, not as you might expect, a T=>S
    *
    * @param f the map function, an S=>T
    *          //@tparam S
    * @return a Predicate[S]
    */
  def map[S](f: S => T): Predicate[S] = new BasePredicate[S](s"$self mapped by $f") {
    def apply(s: S): Boolean = self.apply(f(s))
  }

  def asFunction: T => Boolean = self

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluates if p evaluates as false
    *
    * @param p the other Predicate
    * @return a Predicate which is the disjunctive (and) combination of p with self
    */
  def &:(p: T => Boolean): Predicate[T] = And(p, self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluates if p evaluates as true
    *
    * @param p the other Predicate
    * @return a Predicate which is the conjunctive (or) combination of p with self
    */
  def |:(p: T => Boolean): Predicate[T] = Or(p, self)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the disjunctive (and) combination of self with p
    */
  def :&(p: T => Boolean): Predicate[T] = And(self, p)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as true)
    * @return a Predicate which is the conjunctive (or) combination of self with p
    */
  def :|(p: T => Boolean): Predicate[T] = Or(self, p)
}

abstract class BasePredicate[T](name: String) extends Predicate[T] {
  override def toString = name
}

case class And[T](p1: T => Boolean, p2: T => Boolean) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Boolean = p1(t) && p2(t)
}

case class Or[T](p1: T => Boolean, p2: T => Boolean) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Boolean = p1(t) || p2(t)
}

case class GT[T: Ordering](y: T) extends BasePredicate[T](s">$y") {
  def apply(x: T) = implicitly[Ordering[T]].gt(x, y)
}

case class LT[T: Ordering](y: T) extends BasePredicate[T](s"<$y") {
  def apply(x: T) = implicitly[Ordering[T]].lt(x, y)
}

case class GE[T: Ordering](y: T) extends BasePredicate[T](s">=$y") {
  def apply(x: T) = implicitly[Ordering[T]].gteq(x, y)
}

case class LE[T: Ordering](y: T) extends BasePredicate[T](s"<=$y") {
  def apply(x: T) = implicitly[Ordering[T]].lteq(x, y)
}

case class EQ[T: Ordering](y: T) extends BasePredicate[T](s"=$y") {
  def apply(x: T) = implicitly[Ordering[T]].equiv(x, y)
}

case class NE[T: Ordering](y: T) extends BasePredicate[T](s"!=$y") {
  def apply(x: T) = !implicitly[Ordering[T]].equiv(x, y)
}

case class Func[T](p: T => Boolean) extends BasePredicate[T](s"function $p") {
  def apply(x: T) = p(x)
}

case class Pred[S, T](p: Predicate[T])(f: S => T) extends BasePredicate[S](s"$p with $f") {
  def apply(x: S) = p(f(x))
}

case class In[A](as: Seq[A]) extends BasePredicate[A](s"in ${renderLimited(as)}...") {
  def apply(x: A) = as.contains(x)
}

case class Matches[T](f: PartialFunction[T, Boolean]) extends BasePredicate[T](s"matches $f") {
  def apply(x: T) = f.isDefinedAt(x) && f(x)
}

case class InRange(r: Range) extends BasePredicate[Int](s"in $r") {
  def apply(x: Int) = r.contains(x)
}

case class InBounds[T : Ordering](min: T, max: T) extends BasePredicate[T](s"in bounds $min..$max") {
  def apply(x: T) = (GT(min) :& LT(max))(x)
}

case object Always extends BasePredicate[Any]("true") {
  def apply(t: Any): Boolean = true
}

case object Never extends BasePredicate[Any]("false") {
  def apply(t: Any): Boolean = false
}
