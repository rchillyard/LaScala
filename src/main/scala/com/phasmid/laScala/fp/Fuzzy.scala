package com.phasmid.laScala.fp

trait Fuzzy[+T] {
  /**
    * Get method to return an exact value
    * @return the exact value of this Fuzzy, otherwise an exception is thrown
    *         @throws FuzzyException
    */
  def get: T

  /**
    * Return true if this value is exact
    * @return true if exact, else false
    */
  def isExact: Boolean

  /**
    * Return a measure of the probability that this fuzzy instance could be equal to u.
    * If U is a continuous type, we return the probability density function at u.
    * If U is a discrete type, we return the actual probability that this equals u.
    * @param u
    * @tparam U
    * @return the probability that this is equal to u.
    */
  def pdf[U >: T : Discrete](u: U): Double

  /**
    * Map this fuzzy instance into an instance of Fuzzy[U].
    * @param f
    * @tparam U
    * @return
    */
  def map[U >: T : Discrete](f: T=>U)(implicit ev1: (U=>Double)): Fuzzy[U] = flatMap(t => unit(f(t)))

  /**
    * Map this fuzzy instance into an instance of Fuzzy[U].
    * @param f
    * @tparam U
    * @return
    */
  def flatMap[U](f: T=>Fuzzy[U]): Fuzzy[U]

  def unit[U >: T : Discrete](u: U)(implicit ev1: (U=>Double)): Fuzzy[U]

  def foreach(f: T=>Unit): Unit

}

case class Exact[+T](t: T) extends Fuzzy[T] {
  /**
    * Get method to return an exact value
    *
    * @return the exact value of this Fuzzy, otherwise an exception is thrown
    * @throws FuzzyException
    */
  override def get: T = t

  /**
    * Return true if this value is exact
    *
    * @return true if exact, else false
    */
override def isExact: Boolean = true

  /**
    * Return a measure of the probability that this fuzzy instance could be equal to u.
    * If U is a continuous type, we return the probability density function at u.
    * If U is a discrete type, we return the actual probability that this equals u.
    *
    * @param u
    * @tparam U
    * @return the probability that this is equal to u.
    */
  override def pdf[U >: T : Discrete](u: U): Double = if (u==t) 1 else 0

  /**
    * Map this fuzzy instance into an instance of Fuzzy[U].
    *
    * @param f
    * @tparam U
    * @return
    */
  override def flatMap[U](f: T => Fuzzy[U]): Fuzzy[U] = f(t)

  override def unit[U >: T : Discrete](u: U)(implicit ev1: U => Double): Fuzzy[U] = Exact(u)

  override def foreach(f: T => Unit): Unit = f(t)
}

abstract class ContinuousFuzzy[+T: Fractional](t: T, f: (T)=>Double) extends Fuzzy[T] {
  /**
    * Get method to return an exact value
    *
    * @return the exact value of this Fuzzy, otherwise an exception is thrown
    * @throws FuzzyException
    */
  override def get: T = throw new FuzzyException("non-exact Fuzzy")

  /**
    * Return true if this value is exact
    *
    * @return true if exact, else false
    */
override def isExact: Boolean = false

  /**
    * Map this fuzzy instance into an instance of Fuzzy[U].
    *
    * @param f
    * @tparam U
    * @return
    */
  override def flatMap[U](f: T => Fuzzy[U]): Fuzzy[U] = f(t)

//  override def unit[U >: T : Discrete](u: U)(implicit ev1: U => Double): Fuzzy[U] = Bounded(u, ev1)

  override def foreach(f: T => Unit): Unit = ()

  /**
    * Return a measure of the probability that the actual value of fuzzy instance is equal to u.
    * If U is a continuous type, we return the probability density function at u.
    * If U is a discrete type, we return the actual probability that this equals u.
    *
    * @param u
    * @tparam U
    * @return the probability that this is equal to u.
    */
  override def pdf[U >: T : Discrete](u: U): Double = {
    val ud = implicitly[Discrete[U]]
    val uf = ud.fractional
    val tf = implicitly[Fractional[T]]
    if (ud.isDiscrete(u)) throw new FuzzyException("Bounded does not support discrete variables")
    else {
      println(s"u=${u.getClass}")
      val t = tf.one
//      val d: Double = f(u)
      f(t)
    }
  }

}

case class Bounded[+T: Fractional](t: T, bound: T) extends ContinuousFuzzy[T](t, Bounded.pdf(t,bound)) {


  override def unit[U >: T : Discrete](u: U)(implicit ev1: U => Double): Fuzzy[U] = Bounded(u,u)(implicitly[Discrete[U]].fractional) // FIXME
}

object Bounded {
  def pdf[T : Fractional](nominal: T, bound: T): T=>Double = {
    t =>
      val tf = implicitly[Fractional[T]]
      val z = tf.toDouble(tf.div(tf.one, tf.times(tf.fromInt(2), bound)))
      println(s"z: $z")
      if (tf.compare(tf.abs(tf.minus(nominal,bound)),tf.zero)>=0) z
      else 0
    }

}

class FuzzyException(s: String, t: Throwable = null) extends Exception(s,t)

trait Discrete[T] {
  def isDiscrete(t: T): Boolean
  def fractional: Fractional[T]
}
