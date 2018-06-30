package com.phasmid.laScala.fuzzy

import scala.reflect.ClassTag

trait Fuzzy[T, X] extends (()=>Option[T]) {
  /**
    * Get method to return a value for this fuzzy
    * @return the Some(exact value of this Fuzzy), otherwise None
    */
  def apply(): Option[T]

  /**
    * CONSIDER eliminating this
    * Return true if this value is exact
    * @return true if exact, else false
    */
  def isExact: Boolean = fuzziness.isEmpty

  def fuzziness: Option[Fuzziness[T,X]]

//  /**
//    * Map this fuzzy instance into an instance of Fuzzy[U].
//    * @param f
//    * @tparam U
//    * @return
//    */
//  def map[U](f: T=>U): Fuzzy[U, X]
//
//  /**
//    * Map this fuzzy instance into an instance of Fuzzy[U].
//    * @param f
//    * @tparam U
//    * @return
//    */
//  def flatMap[U, X](f: T=>Fuzzy[U, X]): Fuzzy[U, X]
//
//  def unit(t: T)(implicit ev1: (T=>Double)): Fuzzy[T, X]
//
//  def foreach(f: T=>Unit): Unit

}

case class Exact[T](t: T) extends Fuzzy[T, Nothing] {
  /**
    * Get method to return an exact value
    *
    * @return the exact value of this Fuzzy, otherwise an exception is thrown
    */
  override def apply(): Option[T] = Some(t)

  override def fuzziness: Option[Fuzziness[T, Nothing]] = None

//  /**
//    * Map this fuzzy instance into an instance of Fuzzy[U].
//    *
//    * @param f
//    * @tparam U
//    * @return
//    */
//  override def flatMap[U](f: T => Fuzzy[U]): Fuzzy[U] = f(t)
//
//  override def unit[U >: T : Discrete](u: U)(implicit ev1: U => Double): Fuzzy[U] = Exact(u)
//
//  override def foreach(f: T => Unit): Unit = f(t)
}

abstract class FuzzyBase[T, X](t: T, val fuzziness: Option[Fuzziness[T,X]]) extends Fuzzy[T, X] {
  /**
    * Get method to return a value for this fuzzy
    *
    * @return the Some(exact value of this Fuzzy), otherwise None
    */
  def apply(): Option[T] = None
}

abstract class ContinuousFuzzy[T: Fractional, X](t: T, f: Fuzziness[T,X]) extends FuzzyBase[T, X](t, Some(f)) {

//  /**
//    * Map this fuzzy instance into an instance of Fuzzy[U].
//    *
//    * @param f
//    * @tparam U
//    * @return
//    */
//  override def flatMap[U](f: T => Fuzzy[U]): Fuzzy[U] = f(t)
//
////  override def unit[U >: T : Discrete](u: U)(implicit ev1: U => Double): Fuzzy[U] = Bounded(u, ev1)
//
//  override def foreach(f: T => Unit): Unit = ()
//
}

abstract class ExtendedContinuousFuzzy[T: Fractional](t: T, f: T=>Double) extends ContinuousFuzzy[T, Double](t, ContinuousFuzziness(f)) {

//  protected def getPDF[U >: T : Discrete](u: U): Double = {
//    val ud = implicitly[Discrete[U]]
//    val uf = ud.fractional
//    if (ud.isDiscrete(u)) throw new FuzzyException("Bounded does not support discrete variables")
//    else f(ud.convertTo[T](u))
//  }
}

case class Bounded[T: Fractional: ClassTag](t: T, bound: T) extends ExtendedContinuousFuzzy[T](t, Bounded.pdf(t,bound)) {


//  override def unit[U >: T : Discrete: ClassTag](u: U)(implicit ev1: U => Double): Fuzzy[U] = Bounded(u,u)(implicitly[Discrete[U]].fractional, implicitly[ClassTag[U]]) // FIXME

//  /**
//    * Return a measure of the probability that the actual value of fuzzy instance is equal to u.
//    * If U is a continuous type, we return the probability density function at u.
//    * If U is a discrete type, we return the actual probability that this equals u.
//    *
//    * @param u
//    * @tparam U
//    * @return the probability that this is equal to u.
//    */
//  override def pdf[U >: T : Discrete](u: U): Double = getPDF(u)

}

object Bounded {

  def pdf[T : Fractional](nominal: T, bound: T): T=>Double = {
    val tf = implicitly[Fractional[T]]
    val absBound = tf.abs(bound)
    val lower = tf.minus(nominal,absBound)
    val upper = tf.plus(nominal,absBound)
    import com.phasmid.laScala.fuzzy.CompleteNumeric.CompleteNumericDouble
    Probability.bounded(lower, upper)
  }
//  def pdf[T : Fractional](nominal: T, bound: T): T=>Double = {
//    _ =>
//      val tf = implicitly[Fractional[T]]
//      val z = tf.toDouble(tf.div(tf.one, tf.times(tf.fromInt(2), bound)))
//      println(s"z: $z")
//      if (tf.compare(tf.abs(tf.minus(nominal,bound)),tf.zero)>=0) z
//      else 0
//    }

}

class FuzzyException(s: String, t: Throwable = null) extends Exception(s,t)

trait Discrete[T] {
  def convertTo[U: ClassTag](t: T): U
  def isDiscrete(t: T): Boolean
  def fractional: Fractional[T]
}
