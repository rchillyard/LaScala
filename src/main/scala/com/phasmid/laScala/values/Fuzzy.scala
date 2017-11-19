/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import com.phasmid.laScala.values.Fuzzy.toFractional

/**
  * Typeclass Fuzzy which adds fuzzy behavior to any type.
  *
  * The method apply() will provide the nominal (i.e. most probable) value
  *
  * @tparam T the type to which we add the fuzzy behavior
  */
trait Fuzzy[T] extends (() => T) with Ordered[Fuzzy[T]] {
  def isExact: Boolean

  def getP(t: T): Option[Probability]

  // CONSIDER is there any good reason why U must be a supertype of T?
  def map[U >: T : Fractional](f: T => U): Fuzzy[U]

  /**
    * Method to return EITHER the Probability of the nominal value of this Fuzzy being the same as t.
    * OR (if T is continuous Numeric, such as Double) the probability density function at t.
    *
    * @param t the value of t for which we wish to evaluate the probability of it matching the nominal value.
    * @return either a finite Probability or the value of the probability density function at t.
    */
  def p(t: T): Probability =
    if (isExact)
      if (t == apply()) Probability.Certain else Probability.Impossible
    else getP(t).getOrElse(throw FuzzyException(s"problem with getP for $t"))
}

case class Exact[T: Fractional](nominal: T) extends NumericFuzzy[T](nominal) {

  def isExact: Boolean = true

  def getP(t: T): Option[Probability] = None

  def map[U >: T : Fractional](f: T => U): Fuzzy[U] = Exact[U](f(nominal))

  /**
    * Method to evaluate "map2" for this Fuzzy[T] object
    *
    * @param uf the Fuzzy[U] object which will be the second fuzzy object
    * @param f  the function which takes a T and a U and returns a V
    * @param g  the function which takes a delta-T and a delta-U and returns a delta-V. NOTE: that the coefficients for both the delta-U and the delta-V must be positive.
    * @tparam U the underlying type of the second fuzzy object
    * @tparam V the underlying type of the resulting fuzzy object.
    * @return a Fuzzy[V]
    */
  def map2[U: Fractional, V: Fractional](uf: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): NumericFuzzy[V] = uf match {
    case Exact(u) => Exact(f(nominal, u))
    case Bounded(u, deltaU) => Bounded(f(nominal, u), g(Fuzzy.zero[T], deltaU))
    case _ => throw FuzzyException(s"map2 not supported for $uf")
  }

  def fuzziness: T = Fuzzy.zero
}

case class Bounded[T: Fractional](nominal: T, bound: T) extends NumericFuzzy[T](nominal) {

  import Fuzzy._
  import Probability._

  require(implicitly[Fractional[T]].compare(bound, zero) > 0, "bound must be positive")

  private val min = Fuzzy.minus(nominal, bound)
  private val max = Fuzzy.plus(nominal, bound)

  def getP(t: T): Option[Probability] =
    if (inRange(min, t, max)) Some(Certain * invert(Fuzzy.times(bound, 2)))
    else Some(Impossible)

  def isExact = false

  def map[U >: T : Fractional](f: T => U): Fuzzy[U] = Bounded[U](f(nominal), f(bound))

  def map2[U: Fractional, V: Fractional](uf: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): NumericFuzzy[V] = uf match {
    case Bounded(u, deltaU) => Bounded(f(nominal, u), g(bound, deltaU))
    case Exact(u) => Bounded(f(nominal, u), g(bound, zero[U]))
  }

  def fuzziness: T = bound
}

abstract class NumericFuzzy[T: Fractional](t: T) extends Fuzzy[T] {
  private val tf = implicitly[Fractional[T]]

  import Fuzzy._
  import NumericFuzzy._

  def fuzziness: T

  override def apply(): T = t

  def map2[U: Fractional, V: Fractional](uf: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): NumericFuzzy[V]

  def plus[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = map2(uf)(plusTU[T, U, V], plusTU[T, U, V])

  def minus[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = plus(uf.map(minusT[U]))

  def times[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = map2(uf)(timesTU[T, U, V], timesDerivTU[T, U, V](this, uf))

  def compare(that: Fuzzy[T]): Int = {
    val z = minus(that)
    val p = z.p(tf.zero)
    if (p() > comparisonProbabilityThreshold) 0
    else math.signum(tf.toDouble(z())).toInt
  }
}

object NumericFuzzy {
  private def minusT[T: Fractional](t: T): T = implicitly[Fractional[T]].negate(t)

  private def plusTU[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].plus(toFractional[T, V](t), toFractional[U, V](u))

  private def timesTU[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].times(toFractional[T, V](t), toFractional[U, V](u))

  private def timesDerivTU[T: Fractional, U: Fractional, V: Fractional](tf: Fuzzy[T], uf: Fuzzy[U])(t: T, u: U): V = plusTU[T, U, V](implicitly[Fractional[T]].times(toFractional[U, T](uf()), t), implicitly[Fractional[U]].times(toFractional[T, U](tf()), u))
}

/**
  * Trait which enables Fuzzy[T] to be used in type classes where the context type is a Fractional (or Numeric or Ordering).
  */
trait FuzzyIsFractional[T] extends Fractional[Fuzzy[T]] {

  // Members declared in scala.math.Numeric -- see super methods for scaladoc

  def plus(tf1: Fuzzy[T], tf2: Fuzzy[T]): Fuzzy[T] = tf1 + tf2

  def minus(tf1: Fuzzy[T], tf2: Fuzzy[T]): Fuzzy[T] = tf1 - tf2

  def times(tf1: Fuzzy[T], tf2: Fuzzy[T]): Fuzzy[T] = tf1 * tf2

  def toInt(tf: Fuzzy[T]): Int = tf.toInt

  def toLong(tf: Fuzzy[T]): Long = tf.toLong

  def toFloat(tf: Fuzzy[T]): Float = tf.toFloat

  def toDouble(tf: Fuzzy[T]): Double = tf.toDouble

  //Members declared in scala.math.Fractional
  def div(tf1: Fuzzy[T], tf2: Fuzzy[T]): Fuzzy[T] = tf1 / tf2

  // Members declared in scala.math.Ordering
  def compare(tf1: Fuzzy[T], tf2: Fuzzy[T]): Int = tf1.compare(tf2)
}



object Fuzzy {

  val comparisonProbabilityThreshold = 0.5

  /**
    * TODO do this properly
    *
    * @param m the input value
    * @tparam M the input type
    * @tparam N the output type
    * @return the value converted to the output type
    */
  def toFractional[M: Fractional, N: Fractional](m: M): N = m.asInstanceOf[N]

  def abs[T: Fractional](t: T): T = implicitly[Fractional[T]].abs(t)

  def zero[T: Fractional]: T = implicitly[Fractional[T]].zero

  def fromInt[T: Fractional](x: Int): T = implicitly[Fractional[T]].fromInt(x)

  private def fractional[N: Fractional](x: Int): N = implicitly[Fractional[N]].fromInt(x)

  def negate[N: Numeric](n: N): N = implicitly[Numeric[N]].negate(n)

  def plus[N: Fractional](n1: N, n2: N): N = implicitly[Fractional[N]].plus(n1, n2)

  def minus[N: Fractional](n1: N, n2: N): N = implicitly[Fractional[N]].minus(n1, n2)

  def times[N: Fractional](n1: N, n2: N): N = implicitly[Fractional[N]].times(n1, n2)

  def div[N: Fractional](n1: N, n2: N): N = implicitly[Fractional[N]].div(n1, n2)

  def plus[N: Fractional](n1: N, n2: Int): N = plus(n1, fractional(n2))

  def minus[N: Fractional](n1: N, n2: Int): N = minus(n1, fractional(n2))

  def times[N: Fractional](n1: N, n2: Int): N = times(n1, fractional(n2))

  def invert[N: Fractional](n: N): N = div(fractional(1), n)

  def inRange[T: Numeric](min: T, t: T, max: T): Boolean = {
    val n = implicitly[Numeric[T]]
    n.compare(min, t) <= 0 && n.compare(t, max) <= 0
  }

  //  implicit object FuzzyIsFractionalInt extends FuzzyIsFractional[Int] {
  //    def negate(xf: Fuzzy[Int]): Fuzzy[Int] = xf match {
  //      case Exact(x) => Exact(-x)
  //      case Bounded(x,b) => Bounded(-x,b)
  //    }
  //
  //    def fromInt(x: Int): Fuzzy[Int] = Exact(x)
  //  }

  implicit object FuzzyIsFractionalDouble extends FuzzyIsFractional[Double] {
    def negate(xf: Fuzzy[Double]): Fuzzy[Double] = xf match {
      case Exact(x) => Exact(-x)
      case Bounded(x, b) => Bounded(-x, b)
    }

    def fromInt(x: Int): Fuzzy[Double] = Exact(x)
  }

}

object Bounded {
}

case class FuzzyException(w: String) extends Exception(w)