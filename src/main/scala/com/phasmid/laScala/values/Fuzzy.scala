/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import com.phasmid.laScala.values.Fuzzy.toFractional

import scala.annotation.tailrec

/**
  * Typeclass Fuzzy which adds fuzzy behavior to any type.
  *
  * The method apply() will provide the nominal (i.e. most probable) value
  *
  * @tparam T the type to which we add the fuzzy behavior
  */
trait Fuzzy[T] extends (() => T) with Ordered[Fuzzy[T]] {

  /**
    * Abstract method to determine if this Fuzzy object is exact (i.e. zero fuzziness)
    *
    * @return true if this object is exact, otherwise false
    */
  def isExact: Boolean

  /**
    * Abstract method to determine the probability that the nominal value of this Fuzzy object is actually t
    *
    * @param t the candidate value
    * @return the probability (as an Option) that t=apply(). NOTE: that the value of the probability follows the rules described in method p
    */
  def getP(t: T): Option[Probability]

  /**
    * Abstract method to map this Fuzzy object into a different Fuzzy object.
    *
    * @param f a function T=>U which will be applied to the nominal value of this Fuzzy
    * @param g a function T=>U which will be applied to the fuzzy value of this Fuzzy. NOTE: that the coefficient of delta-T must be positive.
    * @tparam U the underlying type of the result
    * @return a Fuzzy[U] object
    */
  def map[U: Fractional](f: T => U, g: T => U): Fuzzy[U]

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

/**
  * Concrete class extending Fuzzy which has zero fuzziness.
  *
  * @param t the numerical value
  * @tparam T the underlying type of the value: which must support typeclass Fractional
  */
case class Exact[T: Fractional](t: T) extends NumericFuzzy[T](t) {

  def isExact: Boolean = true

  def getP(t: T): Option[Probability] = None

  def map[U: Fractional](f: T => U, g: T => U): Fuzzy[U] = Exact[U](f(t))

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
    case Exact(u) => Exact(f(t, u))
    case Bounded(u, deltaU) => Bounded(f(t, u), g(Fuzzy.zero[T], deltaU))
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
    if (inRange(min, t, max)) Some(Certain * Fuzzy.invert(Fuzzy.times(bound, 2)))
    else Some(Impossible)

  def isExact = false

  def map[U: Fractional](f: T => U, g: T => U): Fuzzy[U] = Bounded[U](f(nominal), g(bound))

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

  /**
    * Abstract method to yield the fuzziness of this NumericFuzzy object.
    * NOTE: that this method is not promoted to Fuzzy because non-numeric Fuzzy objects don't have a concept of fuzziness.
    *
    * @return the degree of fuzziness of this Fuzzy object (as an absolute value)
    */
  def fuzziness: T

  override def apply(): T = t

  /**
    * Abstract method to combine this Fuzzy object with another Fuzzy object
    *
    * @param uf the other fuzzy object, a Fuzzy[U]
    * @param f  the function to process the t and u (nominal) values of this and uf, respectively.
    * @param g  the function to process the deltaT and deltaU (fuzzy) values of this and uf, respectively.
    * @tparam U the underlying type of uf
    * @tparam V the underlying type of the result
    * @return the result
    */
  def map2[U: Fractional, V: Fractional](uf: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): NumericFuzzy[V]

  def plus[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = map2(uf)(plusTU[T, U, V], plusTU[T, U, V])

  def minus[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = plus(uf.asInstanceOf[NumericFuzzy[U]].negate)

  def times[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = map2(uf)(timesTUV[T, U, V], timesDerivTUV[T, U, V](this, uf))

  def negate: NumericFuzzy[T] = map(minusT[T], identity).asInstanceOf[NumericFuzzy[T]]

  // TODO use power (-1) for inversion
  def invert: NumericFuzzy[T] = power(-1)

  def div[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = times(uf.map(invertTV[U, V], inverseDerivTV[U, V](uf)))

  def power(e: Int): NumericFuzzy[T] = map(powerT[T](e), powerDerivT[T](this, e)).asInstanceOf[NumericFuzzy[T]]

  def compare(that: Fuzzy[T]): Int = {
    val z = minus(that)
    val p = z.p(tf.zero)
    if (p() > comparisonProbabilityThreshold) 0
    else math.signum(tf.toDouble(z())).toInt
  }

  def +[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = plus(uf)

  def -[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = minus(uf)

  def *[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = times(uf)

  def /[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = div(uf)
}

object NumericFuzzy {
  private def minusT[T: Fractional](t: T): T = implicitly[Fractional[T]].negate(t)

  private def invertTV[T: Fractional, V: Fractional](t: T): V = {
    val f = implicitly[Fractional[T]]
    toFractional[T, V](f.div(f.fromInt(1), t))
  }

  private def plusTU[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].plus(toFractional[T, V](t), toFractional[U, V](u))

  private def timesTUV[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].times(toFractional[T, V](t), toFractional[U, V](u))

  private def timesDerivTUV[T: Fractional, U: Fractional, V: Fractional](tf: Fuzzy[T], uf: Fuzzy[U])(t: T, u: U): V = plusTU[T, U, V](implicitly[Fractional[T]].times(toFractional[U, T](uf()), t), implicitly[Fractional[U]].times(toFractional[T, U](tf()), u))

  private def inverseDerivTV[T: Fractional, V: Fractional](tf: Fuzzy[T])(t: T): V = {
    val f = implicitly[Fractional[T]]
    toFractional[T, V](f.div(f.fromInt(1), f.times(tf(), tf())))
  }

  private def powerDerivT[T: Fractional](tf: Fuzzy[T], k: Int)(t: T): T = {
    val f = implicitly[Fractional[T]]
    f.times(Fuzzy.exp(t, k - 1), f.fromInt(k))
  }

  private def powerT[T: Fractional](e: Int)(t: T): T = Fuzzy.exp(t, e)
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

  def exp[N: Fractional](n: N, x: Int): N = {
    val f = implicitly[Fractional[N]]

    @tailrec def inner(r: N, k: Int): N = if (k == 0) r else inner(f.times(n, r), k - 1)

    if (x >= 0) inner(f.fromInt(1), x) else invert(inner(f.fromInt(1), math.abs(x)))
  }

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