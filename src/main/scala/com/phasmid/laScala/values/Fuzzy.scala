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

  def map2[U: Fractional, V: Fractional](x: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): Fuzzy[V] = x match {
    case Exact(v) => Exact(f(nominal, v))
    case Bounded(v, deltaV) => Bounded(f(nominal, v), g(Fuzzy.fromInt[T](0), deltaV))
    case _ => throw FuzzyException(s"map2 not supported for $x")
  }

  def fuzziness: T = implicitly[Fractional[T]].zero
}

case class Bounded[T: Fractional](nominal: T, bound: T) extends NumericFuzzy[T](nominal) {

  import Bounded._
  import Fuzzy._
  import Probability._

  private val min = Bounded.minus(nominal, bound)
  private val max = Bounded.plus(nominal, bound)

  def getP(t: T): Option[Probability] =
    if (inRange(min, t, max)) Some(Certain * invert(Bounded.times(bound, 2)))
    else Some(Impossible)

  def isExact = false

  def map[U >: T : Fractional](f: T => U): Fuzzy[U] = Bounded[U](f(nominal), f(bound))

  def map2[U: Fractional, V: Fractional](x: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): Fuzzy[V] = x match {
    case Bounded(v, deltaV) => Bounded(f(nominal, v), g(bound, deltaV))
    case Exact(v) => Bounded(f(nominal, v), g(bound, fromInt[U](0)))
  }

  def fuzziness: T = bound
}

abstract class NumericFuzzy[T: Fractional](t: T) extends Fuzzy[T] {
  private val tf = implicitly[Fractional[T]]

  import Fuzzy._
  import NumericFuzzy._

  def fuzziness: T

  override def apply(): T = t

  def map2[U: Fractional, V: Fractional](x: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): Fuzzy[V]

  def plus[U: Fractional, V: Fractional](x: Fuzzy[U]): Fuzzy[V] = map2(x)(plusXY[T, U, V], plusXY[T, U, V])

  def minus[U: Fractional, V: Fractional](x: Fuzzy[U]): Fuzzy[V] = plus(x.map(minusX[U]))

  def times[U: Fractional, V: Fractional](x: Fuzzy[U]): Fuzzy[V] = map2(x)(timesXY[T, U, V], (t, u) => plusXY[T, U, V](implicitly[Fractional[T]].times(toFractional[U, T](x()), t), implicitly[Fractional[U]].times(toFractional[T, U](apply()), u)))

  def compare(that: Fuzzy[T]): Int = {
    val z = minus(that)
    val p = z.p(tf.zero)
    if (p() > comparisonProbabilityThreshold) 0
    else math.signum(tf.toDouble(z())).toInt
  }

}

object NumericFuzzy {
  private def minusX[U: Fractional](u: U): U = implicitly[Fractional[U]].negate(u)

  private def plusXY[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].plus(toFractional[T, V](t), toFractional[U, V](u))

  private def timesXY[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].times(toFractional[T, V](t), toFractional[U, V](u))

  private def timesDerivXY[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].plus(toFractional[T, V](t), toFractional[U, V](u))

}

/**
  * Trait which enables Rational[N] to be used in type classes where the context type is a Fractional (or Numeric or Ordering).
  */
trait FuzzyIsFractional[T] extends Fractional[Fuzzy[T]] {

  // Members declared in scala.math.Numeric -- see super methods for scaladoc

  def plus(x: Fuzzy[T], y: Fuzzy[T]): Fuzzy[T] = x + y

  def minus(x: Fuzzy[T], y: Fuzzy[T]): Fuzzy[T] = x - y

  def times(x: Fuzzy[T], y: Fuzzy[T]): Fuzzy[T] = x * y

  def toInt(x: Fuzzy[T]): Int = x.toInt

  def toLong(x: Fuzzy[T]): Long = x.toLong

  def toFloat(x: Fuzzy[T]): Float = x.toFloat

  def toDouble(x: Fuzzy[T]): Double = x.toDouble

  //Members declared in scala.math.Fractional
  def div(x: Fuzzy[T], y: Fuzzy[T]): Fuzzy[T] = x / y

  // Members declared in scala.math.Ordering
  def compare(x: Fuzzy[T], y: Fuzzy[T]): Int = x.compare(y)
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

  def fromInt[T: Fractional](x: Int): T = implicitly[Fractional[T]].fromInt(x)

  def inRange[T: Numeric](x: T, y: T, z: T): Boolean = {
    val n = implicitly[Numeric[T]]
    n.compare(x, y) <= 0 && n.compare(y, z) <= 0
  }

  implicit object FuzzyIsFractionalInt extends FuzzyIsFractional[Int] {
    def negate(x: Fuzzy[Int]): Fuzzy[Int] = ???

    def fromInt(x: Int): Fuzzy[Int] = ???
  }

  implicit object FuzzyIsFractionalDouble extends FuzzyIsFractional[Double] {
    def negate(x: Fuzzy[Double]): Fuzzy[Double] = ???

    def fromInt(x: Int): Fuzzy[Double] = ???
  }

}

object Bounded {
  def fractional[N: Fractional](x: Int): N = implicitly[Fractional[N]].fromInt(x)

  def plus[N: Fractional](n: N, x: N): N = implicitly[Fractional[N]].plus(n, x)

  def minus[N: Fractional](n: N, x: N): N = implicitly[Fractional[N]].minus(n, x)

  def times[N: Fractional](n: N, x: N): N = implicitly[Fractional[N]].times(n, x)

  def div[N: Fractional](n: N, x: N): N = implicitly[Fractional[N]].div(n, x)

  def plus[N: Fractional](n: N, x: Int): N = plus(n, fractional(x))

  def minus[N: Fractional](n: N, x: Int): N = minus(n, fractional(x))

  def times[N: Fractional](n: N, x: Int): N = times(n, fractional(x))

  def invert[N: Fractional](n: N): N = div(fractional(1), n)
}

case class FuzzyException(w: String) extends Exception(w)