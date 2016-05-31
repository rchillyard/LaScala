package com.phasmid.laScala

import com.phasmid.laScala.FP._
import com.phasmid.laScala.parser.{PredicateExpr, Variable, Number}

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
    * The transform function for Predicate. Similar to map but the
    * parameter f is an S=>T, that's to say the inverse of a T=>S
    *
    * @param f the transform function, an S=>T
    *          //@tparam S
    * @return a Predicate[S]
    */
  def transform[S](f: S => T): Predicate[S] = new BasePredicate[S](s"$self transformed by $f") {
    def apply(s: S): Boolean = self.apply(f(s))
  }

  def asFunction: T => Boolean = self

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if p evaluates as false
    *
    * @param p the other Predicate
    * @return a Predicate which is the conjunctive (and) combination of p with self
    */
  def &:(p: T => Boolean): Predicate[T] = And(p, self)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if p evaluates as true
    *
    * @param p the other Predicate
    * @return a Predicate which is the disjunctive (or) combination of p with self
    */
  def |:(p: T => Boolean): Predicate[T] = Or(p, self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of self with p
    */
  def :&(p: T => Boolean): Predicate[T] = And(self, p)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as true)
    * @return a Predicate which is the disjunctive (or) combination of self with p
    */
  def :|(p: T => Boolean): Predicate[T] = Or(self, p)
}

abstract class BasePredicate[T](name: String) extends Predicate[T] {
  override def toString = name
}

/**
  * "And" sub-class of BasePredicate yielding the conjunction of p1 and p2.
  * @param p1 a function T=>Boolean which will typically be a Predicate; this function will always be evaluated
  * @param p2 a function T=>Boolean which will typically be a Predicate; this function may not be evaluated
  * //@tparam T
  */
case class And[T](p1: T => Boolean, p2: T => Boolean) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Boolean = p1(t) && p2(t)
}

/**
  * "Or" sub-class of BasePredicate yielding the disjunction of p1 and p2.
  * @param p1 a function T=>Boolean which will typically be a Predicate; this function will always be evaluated
  * @param p2 a function T=>Boolean which will typically be a Predicate; this function may not be evaluated
  * //@tparam T
  */
case class Or[T](p1: T => Boolean, p2: T => Boolean) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Boolean = p1(t) || p2(t)
}

/**
  * "GT" sub-class of BasePredicate which evaluates to true if x > y where x is the parameter passed into the apply method.
  * @param y a T value
  * //@tparam T
  */
case class GT[T: Ordering](y: T) extends BasePredicate[T](s">$y") {
  def apply(x: T) = implicitly[Ordering[T]].gt(x, y)
}

/**
  * "LT" sub-class of BasePredicate which evaluates to true if x < y where x is the parameter passed into the apply method.
  * @param y a T value
  * //@tparam T
  */
case class LT[T: Ordering](y: T) extends BasePredicate[T](s"<$y") {
  def apply(x: T) = implicitly[Ordering[T]].lt(x, y)
}

/**
  * "GE" sub-class of BasePredicate which evaluates to true if x >= y where x is the parameter passed into the apply method.
  * @param y a T value
  * //@tparam T
  */
case class GE[T: Ordering](y: T) extends BasePredicate[T](s">=$y") {
  def apply(x: T) = implicitly[Ordering[T]].gteq(x, y)
}

/**
  * "LE" sub-class of BasePredicate which evaluates to true if x <= y where x is the parameter passed into the apply method.
  * @param y a T value
  * //@tparam T
  */
case class LE[T: Ordering](y: T) extends BasePredicate[T](s"<=$y") {
  def apply(x: T) = implicitly[Ordering[T]].lteq(x, y)
}

/**
  * "EQ" sub-class of BasePredicate which evaluates to true if x = y where x is the parameter passed into the apply method.
  * @param y a T value
  * //@tparam T
  */
case class EQ[T: Ordering](y: T) extends BasePredicate[T](s"=$y") {
  def apply(x: T) = implicitly[Ordering[T]].equiv(x, y)
}

/**
  * "NE" sub-class of BasePredicate which evaluates to true if x != y where x is the parameter passed into the apply method.
  * @param y a T value
  * //@tparam T
  */
case class NE[T: Ordering](y: T) extends BasePredicate[T](s"!=$y") {
  def apply(x: T) = !implicitly[Ordering[T]].equiv(x, y)
}

/**
  * "Func" sub-class of BasePredicate which evaluates to true if p(x) where x is the parameter passed into the apply method.
  * @param p a T=> Boolean function (might be another predicate)
  * //@tparam T
  */
case class Func[T](p: T => Boolean) extends BasePredicate[T](s"function $p") {
  def apply(x: T) = p(x)
}

/**
  * "Pred" sub-class of BasePredicate which evaluates to true if p(f(x)) where x is the parameter passed into the apply method.
  * @param p a Predicate
  * @param f a S=>T function
  * //@tparam T
  */
case class Pred[S, T](p: Predicate[T])(f: S => T) extends BasePredicate[S](s"$p with $f") {
  def apply(x: S) = p(f(x))
}

/**
  * "In" sub-class of BasePredicate which evaluates to true if as contains x where x is the parameter passed into the apply method.
  * @param as a sequence of A values
  * //@tparam A
  */
case class In[A](as: Seq[A]) extends BasePredicate[A](s"in ${renderLimited(as)}...") {
  def apply(x: A) = as.contains(x)
}

/**
  * "Matches" sub-class of BasePredicate which evaluates to true if f.isDefinedAt(x) and f(x) where x is the parameter passed into the apply method.
  * @param f a T => Boolean partial function
  * //@tparam T
  */
case class Matches[T](f: PartialFunction[T, Boolean]) extends BasePredicate[T](s"matches $f") {
  def apply(x: T) = f.isDefinedAt(x) && f(x)
}

/**
  * "InRange" sub-class of BasePredicate[Int] which evaluates to true if r contains x where x is the parameter passed into the apply method.
  * @param r a Range
  */
case class InRange(r: Range) extends BasePredicate[Int](s"in $r") {
  def apply(x: Int) = r.contains(x)
}

/**
  * "InBounds" sub-class of BasePredicate which evaluates to true if x > y where x is the parameter passed into the apply method.
  * @param min a T value
  * @param max a T value
  * //@tparam T
  */
case class InBounds[T : Ordering](min: T, max: T) extends BasePredicate[T](s"in bounds $min..$max") {
  def apply(x: T) = (GT(min) :& LT(max))(x)
}

/**
  * "Always" sub-class of BasePredicate which evaluates to true.
  * //@tparam T
  */
case object Always extends BasePredicate[Any]("true") {
  def apply(t: Any): Boolean = true
}

/**
  * "Never" sub-class of BasePredicate which evaluates to false.
  * //@tparam T
  */
case object Never extends BasePredicate[Any]("false") {
  def apply(t: Any): Boolean = false
}

class PredicateException(s: String) extends Exception(s"rule problem: $s")

object Predicate {
  implicit def convertFromPredicateExpr(x: PredicateExpr): Predicate[String] = {
    val p: String = x.operand match {
      case v: Variable => v.s
      case n: Number => n.s+n.m
    }
    getPredicate(x, p)
  }
  implicit def convertFromPredicateExpr(x: PredicateExpr)(implicit m: String=>Double): Predicate[Double] = {
    val p: Double = x.operand match {
      case v: Variable =>
        import Variable._
        v
      case n: Number =>
        import Number._
        n
    }
    getPredicate(x, p)
  }
  //  implicit def convertToTPredicate(x: PredicateExpr)(implicit m: String=>Int): Predicate[Int] = {
  //    val p: Int = x.operand match {
  //      case v: Variable =>
  //        import Variable._
  //        v
  //      case n: Number =>
  //        import Number._
  //        n
  //    }
  //    getPredicate(x, p)
  //  }
  private def getPredicate[T : Ordering](x: PredicateExpr, p: T): Predicate[T] = {
    x.operator match {
      case ">" => GT(p)
      case ">=" => GE(p)
      case "<" => LT(p)
      case "<=" => LE(p)
      case "=" => EQ(p)
      case "!=" => NE(p)
      case _ => throw new PredicateException(s"NYI: $x")
    }
  }
}
