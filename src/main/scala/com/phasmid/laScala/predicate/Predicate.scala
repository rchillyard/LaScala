package com.phasmid.laScala.predicate

import com.phasmid.laScala.FP._
import com.phasmid.laScala.parser._

import scala.util.{Failure, Success, Try}

/**
  * This trait extends the function class T=>Try[Boolean].
  * Predicates are intended to combine and form rules.
  * At present, a Predicate is a Functor but not a Monad.
  *
  * Created by robinhillyard on 5/24/16.
  */
trait Predicate[T] extends (T => Try[Boolean]) {
  self =>

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  def map[U: Ordering](f: T => U): Predicate[U]

  def asFunction: T => Try[Boolean] = self

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
  def &:(p: T => Try[Boolean]) = And(Predicate(p), self)

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
  def &:(p: Predicate[T]) = And(Predicate(p), self)

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
  def |:(p: T => Try[Boolean]) = Or(Predicate(p), self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of self with p
    */
  def :&(p: T => Try[Boolean]) = And(self, Predicate(p))

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as true)
    * @return a Predicate which is the disjunctive (or) combination of self with p
    */
  def :|(p: T => Try[Boolean]) = Or(self, Predicate(p))

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if f evaluates as false
    *
    * @param f the other function T=>Boolean (f will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of f with self
    */
  def &^:(f: T => Boolean) = And(Func(f), self)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if f evaluates as true
    *
    * @param f a T=>Boolean function
    * @return a Predicate which is the disjunctive (or) combination of p with self
    */
  def |^:(f: T => Boolean) = Or(Func(f), self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param f the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of self with p
    */
  def :^&(f: T => Boolean) = And(self, Func(f))

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param f the other function T=>Boolean (f will not be evaluated if self evaluates as true)
    * @return a Predicate which is the disjunctive (or) combination of self with f
    */
  def :^|(f: T => Boolean) = Or(self, Func(f))
}

abstract class BasePredicate[T](name: String) extends Predicate[T] { self =>
  override def toString = name
}

/**
  * "And" sub-class of BasePredicate yielding the conjunction of p1 and p2.
  * @param p1 a function T=>Try[Boolean] which will typically be a Predicate; this function will always be evaluated
  * @param p2 a function T=>Try[Boolean] which will typically be a Predicate; this function may not be evaluated
  * //@tparam T
  */
case class And[T](p1: Predicate[T], p2: Predicate[T]) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Try[Boolean] = map2lazy(p1(t),p2(t))(_&&_)({x => x}, Success(false))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = And(p1 map f, p2 map f)
}

/**
  * "Or" sub-class of BasePredicate yielding the disjunction of p1 and p2.
  * @param p1 a function T=>Try[Boolean] which will typically be a Predicate; this function will always be evaluated
  * @param p2 a function T=>Try[Boolean] which will typically be a Predicate; this function may not be evaluated
  * //@tparam T
  */
case class Or[T](p1: Predicate[T], p2: Predicate[T]) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Try[Boolean] = map2lazy(p1(t),p2(t))(_||_)({x => !x}, Success(true))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = Or(p1 map f, p2 map f)
}

/**
  * "GT" sub-class of BasePredicate which evaluates to true if x > y where x is the parameter passed into the apply method.
  * @param y a T expression
  * //@tparam T
  */
case class GT[T: Ordering](y: T) extends BasePredicate[T](s">$y") { self =>
  def apply(x: T) = Try(implicitly[Ordering[T]].gt(x, y))
  def map[U: Ordering](f: (T) => U) = new GT(f(y)) {
    override def toString = s">$y mapped by $f"
  }
}

/**
  * "LT" sub-class of BasePredicate which evaluates to true if x < y where x is the parameter passed into the apply method.
  * @param y a T expression
  * //@tparam T
  */
case class LT[T: Ordering](y: T) extends BasePredicate[T](s"<$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].lt(x, y))
  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = LT(f(y))
}

/**
  * "GE" sub-class of BasePredicate which evaluates to true if x >= y where x is the parameter passed into the apply method.
  * @param y a T expression
  * //@tparam T
  */
case class GE[T: Ordering](y: T) extends BasePredicate[T](s">=$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].gteq(x, y))
  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = GE(f(y))
}

/**
  * "LE" sub-class of BasePredicate which evaluates to true if x <= y where x is the parameter passed into the apply method.
  * @param y a T expression
  * //@tparam T
  */
case class LE[T: Ordering](y: T) extends BasePredicate[T](s"<=$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].lteq(x, y))
  /**
    * The transform function for Predicate. Similar to map but the
    * parameter f is an S=>T, that's to say the inverse of a T=>S
    *
    * @param f the transform function, an S=>T
    *          //@tparam S
    * @return a Predicate[S]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = LE(f(y))
}

/**
  * "EQ" sub-class of BasePredicate which evaluates to true if x = y where x is the parameter passed into the apply method.
  * @param y a T expression
  * //@tparam T
  */
case class EQ[T: Ordering](y: T) extends BasePredicate[T](s"=$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].equiv(x, y))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = EQ(f(y))
}

/**
  * "NE" sub-class of BasePredicate which evaluates to true if x != y where x is the parameter passed into the apply method.
  * @param y a T expression
  * //@tparam T
  */
case class NE[T: Ordering](y: T) extends BasePredicate[T](s"!=$y") {
  def apply(x: T) = Try(!implicitly[Ordering[T]].equiv(x, y))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = NE(f(y))
}

/**
  * "Func" sub-class of BasePredicate which evaluates to true if p(x) where x is the parameter passed into the apply method.
  * @param p a T=> Boolean function (might be another predicate)
  * //@tparam T
  */
case class Func[T](p: T => Boolean) extends BasePredicate[T](s"function $p") {
  def apply(x: T) = Try(p(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = throw new PredicateException("NYI Func.map")
}

/**
  * "Pred" sub-class of BasePredicate which evaluates to true if p(f(x)) where x is the parameter passed into the apply method.
  * @param p a Predicate
  * @param f a S=>T function
  * //@tparam T
  */
case class Pred[T, V](p: Predicate[V])(f: T => V) extends BasePredicate[T](s"$p with $f") {
  def apply(x: T) = p(f(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param g the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](g: (T) => U): Predicate[U] = throw new PredicateException("NYI Pred.map")
}

/**
  * "In" sub-class of BasePredicate which evaluates to true if as contains x where x is the parameter passed into the apply method.
  * @param as a sequence of A values
  * //@tparam A
  */
case class In[A](as: Seq[A]) extends BasePredicate[A](s"in ${renderLimited(as)}...") {
  def apply(x: A) = Try(as.contains(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (A) => U): Predicate[U] = throw new PredicateException("NYI In.map")
}

/**
  * "Matches" sub-class of BasePredicate which evaluates to true if f.isDefinedAt(x) and f(x) where x is the parameter passed into the apply method.
  * @param f a T => Boolean partial function
  * //@tparam T
  */
case class Matches[T](f: PartialFunction[T, Boolean]) extends BasePredicate[T](s"matches $f") {
  def apply(x: T) = Try(f.isDefinedAt(x) && f(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = throw new PredicateException("NYI Matches.map")
}

/**
  * "InRange" sub-class of BasePredicate[Int] which evaluates to true if r contains x where x is the parameter passed into the apply method.
  * @param r a Range
  */
case class InRange(r: Range) extends BasePredicate[Int](s"in $r") {
  def apply(x: Int) = Try(r.contains(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (Int) => U): Predicate[U] = throw new PredicateException("NYI InRange.map")
}

/**
  * "InBounds" sub-class of BasePredicate which evaluates to true if x > y where x is the parameter passed into the apply method.
  * @param min a T expression
  * @param max a T expression
  * //@tparam T
  */
case class InBounds[T : Ordering](min: T, max: T) extends BasePredicate[T](s"in bounds $min..$max") {
  def apply(x: T) = (GT(min) :& LT(max))(x)

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (T) => U): Predicate[U] = throw new PredicateException("NYI InBounds.map")
}

/**
  * "Always" sub-class of BasePredicate which evaluates to true.
  * //@tparam T
  */
case object Always extends BasePredicate[Any]("true") {
  def apply(t: Any): Try[Boolean] = Success(true)

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (Any) => U): Predicate[U] = new BasePredicate[U]("Always mapped") {/**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[V: Ordering](f: (U) => V): Predicate[V] = throw new PredicateException("NYI Always.map.map")

    override def apply(v1: U): Try[Boolean] = Success(true)
  }
}

/**
  * "Never" sub-class of BasePredicate which evaluates to false.
  * //@tparam T
  */
case object Never extends BasePredicate[Any]("false") {
  def apply(t: Any): Try[Boolean] = Success(false)

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (Any) => U): Predicate[U] = throw new PredicateException("NYI Never.map")
}

case class InvalidPredicate(x: Throwable) extends BasePredicate[Any](s"invalid: $x") {
  def apply(t: Any): Try[Boolean] = Failure(x)

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    *          //@tparam U
    * @return a Predicate[U]
    */
  override def map[U: Ordering](f: (Any) => U): Predicate[U] = InvalidPredicate(x).asInstanceOf[Predicate[U]]
}
class PredicateException(s: String) extends Exception(s"rule problem: $s")

object Predicate {
  def apply[T](p: T=>Try[Boolean]): Predicate[T] = new BasePredicate[T](s"$p") { self =>
    /**
      * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
      * that the mapping between T and U be isomorphic.
      *
      * This method will fail where the predicate does not rely on Ordering.
      *
      * @param f the map function, an T=>U
      *          //@tparam U
      * @return a Predicate[U]
      */
    override def map[U: Ordering](f: (T) => U): Predicate[U] = throw new PredicateException("NYI apply.map")

    override def apply(t: T): Try[Boolean] = p(t)
  }
  implicit def convertFromPredicateExpr(x: PredicateExpr): Predicate[String] = {
    // XXX for now, we will just turn an RPN list into a String
    val p: String = x.operand.toRPN.mkString(" ")
    getPredicate(x, p)
  }
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
