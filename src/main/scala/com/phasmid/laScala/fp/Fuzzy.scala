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
  def flatMap[U >: T : Discrete](f: T=>Fuzzy[U]): Fuzzy[U]

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
  override def flatMap[U >: T : Discrete](f: T => Fuzzy[U]): Fuzzy[U] = f(t)

  override def unit[U >: T : Discrete](u: U)(implicit ev1: U => Double): Fuzzy[U] = Exact(u)

  override def foreach(f: T => Unit): Unit = f(t)
}

class FuzzyException(s: String, t: Throwable) extends Exception(s,t)

trait Discrete[T] {
  def isDiscrete(t: T): Boolean
}
