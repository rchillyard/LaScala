package com.phasmid.laScala

/**
  * Created by robinhillyard on 5/24/16.
  */
trait Predicate[T] extends ((T)=>Boolean) { self =>
  /**
    * The map function for Predicate.
    * Note that the parameter f is an S=>T, not as you might expect, a T=>S
    * @param f the map function, an S=>T
    * //@tparam S
    * @return a Predicate[S]
    */
  def map[S](f: S=>T): Predicate[S] = new BasePredicate[S] {
    def apply(s: S): Boolean = self.apply(f(s))
  }
//  def &:[U <: T](p: Predicate[U]): Predicate[T=>U] = And(this,p)
//  def |:[U <: T](p: Predicate[T]): Predicate[T=>U] = Or(this,p)
//  def :&[U <: T](p: Predicate[T]): Predicate[T=>U] = And(this,p)
//  def :|[U <: T](p: Predicate[T]): Predicate[T=>U] = Or(this,p)
}

abstract class BasePredicate[T] extends Predicate[T]

case class GT[T : Ordering](y: T) extends BasePredicate[T] {
  def apply(x: T) = (implicitly[Ordering[T]]).gt(x,y)
}
case class LT[T : Ordering](y: T) extends BasePredicate[T] {
  def apply(x: T) = (implicitly[Ordering[T]]).lt(x,y)
}
case class GE[T : Ordering](y: T) extends BasePredicate[T] {
  def apply(x: T) = (implicitly[Ordering[T]]).gteq(x,y)
}
case class LE[T : Ordering](y: T) extends BasePredicate[T] {
  def apply(x: T) = (implicitly[Ordering[T]]).lteq(x,y)
}
case class EQ[T : Ordering](y: T) extends BasePredicate[T] {
  def apply(x: T) = (implicitly[Ordering[T]]).equiv(x,y)
}
case class NE[T : Ordering](y: T) extends BasePredicate[T] {
  def apply(x: T) = !(implicitly[Ordering[T]]).equiv(x,y)
}
case class Func[T](p: T=>Boolean) extends BasePredicate[T] {
  def apply(x: T) = p(x)
}
case class Pred[S,T](p: Predicate[T])(f: S=>T) extends BasePredicate[S] {
  def apply(x: S) = p(f(x))
}
case class In[A](as: Seq[A]) extends BasePredicate[A] {
  def apply(x: A) = as.contains(x)
}
case class Matches[T](f: PartialFunction[T,Boolean]) {
  def apply(x: T) = f.isDefinedAt(x) && f(x)
}
case class InRange(r: Range) extends BasePredicate[Int] {
  def apply(x: Int) = r.contains(x)
}
case object Always extends Predicate[Any] {
  def apply(t: Any): Boolean = true
}
case object Never extends Predicate[Any] {
  def apply(t: Any): Boolean = false
}

//case class And[T,U <: T](p1: Predicate[T], p2: Predicate[U]) extends Predicate[T=>U] {
//  def apply(f: T=>U): Boolean = {
//    def g(t: T) = if (p1(t)) p2(_) else Never()}
//}
////{
////  def apply(u: U): Boolean = if (p1 _) p2(u) else Never()
////}
//
//case class Or[T,U <: T](p1: Predicate[T], p2: Predicate[U]) extends Predicate[U] {
//  def apply(u: U): Boolean = if (p1 _) Always() else p2(u)
//}
