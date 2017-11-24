/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import com.phasmid.laScala.fp.FP
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
case class Exact[T: Fractional](t: T) extends BaseNumericFuzzy[T](t) {

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
    case bnf: BaseNumericFuzzy[U] => bnf.map2(this)(flip(f), flip(g))
    case _ => throw FuzzyException(s"map2 not supported for $this and $uf")
  }

  def fuzziness: T = Fuzzy.zero

  override def toString(): String = s"$t"

  def p(t1: T, t2: T)(implicit ev: Ordering[T]): Probability = if (Fuzzy.inRange(t1, t, t2)) Probability.Certain else Probability.Impossible

}

case class Bounded[T: Fractional](nominal: T, bound: T) extends BaseNumericFuzzy[T](nominal) {

  import Fuzzy._
  import Probability._

  private val frT: Fractional[T] = implicitly[Fractional[T]]
  require(frT.compare(bound, zero) > 0, "bound must be positive")

  private val min = Fuzzy.minus(nominal, bound)
  private val max = Fuzzy.plus(nominal, bound)

  def getP(t: T): Option[Probability] =
    if (inRange(t)) Some(Certain * Fuzzy.invert(Fuzzy.times(bound, 2)))
    else Some(Impossible)

  override def compare(that: Fuzzy[T]): Int = minus(that) match {
    case b: Bounded[T] => if (b.inRange(frT.zero)) 0 else super.compareTo(that)
    case _ => super.compareTo(that)
  }

  def isExact = false

  def map[U: Fractional](f: T => U, g: T => U): Fuzzy[U] = Bounded[U](f(nominal), implicitly[Fractional[U]].abs(g(bound)))

  def map2[U: Fractional, V: Fractional](uf: Fuzzy[U])(f: (T, U) => V, g: (T, U) => V): NumericFuzzy[V] = uf match {
    case Bounded(u, deltaU) => Bounded(f(nominal, u), g(bound, deltaU))
    case Exact(u) => Bounded(f(nominal, u), g(bound, zero[U]))
    case _ => throw FuzzyException(s"map2 not supported for $this and $uf")
  }

  def fuzziness: T = bound

  override def toString(): String = s"$nominal +- $bound"

  def p(t1: T, t2: T)(implicit ev: Ordering[T]): Probability = {
    val orT = implicitly[Ordering[T]]
    val range = frT.minus(orT.min(t2, max), orT.max(t1, min))
    // NOTE: that we don't keep Rational intact: every type of number gets converted to Double
    Probability(frT.toDouble(frT.div(range, frT.times(frT.fromInt(2), bound))))
  }

  private def inRange(t: T) = Fuzzy.inRange(min, t, max)
}

trait NumericFuzzy[T] extends Fuzzy[T] {

  /**
    * Abstract method to yield the fuzziness of this NumericFuzzy object.
    * NOTE: that this method is not promoted to Fuzzy because non-numeric Fuzzy objects don't have a concept of fuzziness.
    *
    * @return the degree of fuzziness of this Fuzzy object (as an absolute value)
    */
  def fuzziness: T

  /**
    * Abstract method to determine the probability that the nominal value of this Fuzzy object is
    * between t1 and t2 (inclusive).
    *
    * @param t1 the lower value
    * @param t2 the higher value
    * @return a probability
    */
  def p(t1: T, t2: T)(implicit ev: Ordering[T]): Probability

}

abstract class BaseNumericFuzzy[T: Fractional](t: T) extends NumericFuzzy[T] {
  self =>
  private val frT = implicitly[Fractional[T]]

  import BaseNumericFuzzy._
  import Fuzzy._

  override def apply(): T = t

  /**
    * Method to yield the nominal value as a Double
    *
    * @return the nominal value of this NumericFuzzy.
    */
  def toDouble: Double = implicitly[Numeric[T]].toDouble(apply())

  /**
    * Convenience method to invoke map on a NumericFuzzy such that the result is also a NumericFuzzy
    *
    * @param f      a function T=>U which will be applied to the nominal value of this Fuzzy
    * @param dfbydx a function T=>U which will be applied to the nominal value of this Fuzzy.
    * @tparam U the underlying type of the result
    * @return a NumericFuzzy[U] object
    */
  def mapFunc[U: Fractional](f: T => U, dfbydx: T => U): NumericFuzzy[U] = map(f, { dx =>
    val uf = implicitly[Fractional[U]]
    uf.abs(uf.times(dfbydx(apply()), toFractional[T, U](dx)))
  }
  ).asInstanceOf[NumericFuzzy[U]]

  //  def mapNumeric[U: Fractional](f: T => U, g: T => U): NumericFuzzy[U] = new MappedFuzzy[U](f, g, isExact).asInstanceOf[NumericFuzzy[U]]

  /**
    * Convenience method to invoke map on a NumericFuzzy such that the result is also a NumericFuzzy,
    * but with the possibility of creating an Exact(1)
    *
    * @param f      a function T=>U which will be applied to the nominal value of this Fuzzy
    * @param dfbydx a function T=>U which will be applied to the nominal value of this Fuzzy.
    * @param unity  if true then an Exact(1) will be returned, otherwise we delegate to mapFunc
    * @tparam U the underlying type of the result
    * @return a NumericFuzzy[U] object
    */
  def mapFuncExp[U: Fractional](f: T => U, dfbydx: T => U)(unity: Boolean): NumericFuzzy[U] = if (unity) Exact[U](1)
  else mapFunc(f, dfbydx)

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

  def minus[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = plus(uf.asInstanceOf[BaseNumericFuzzy[U]].negate)

  def times[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = map2(uf)(timesTUV[T, U, V], timesDerivTUV[T, U, V](this, uf))

  //  def exp[U: Fractional](uf: NumericFuzzy[U]): NumericFuzzy[Double] = {
  //    val e = uf.toDouble
  //    // TODO add in the error from uf
  //    mapFuncExp(powerTx(e), x => powerTx(e - 1)(tf.toDouble(x)) * e )(uf.isExact && e == 0)
  //  }

  def negate: NumericFuzzy[T] = mapFunc(minusT[T], { _ => frT.fromInt(-1) })

  def invert: NumericFuzzy[T] = power(-1)

  def div[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = times(uf.asInstanceOf[BaseNumericFuzzy[U]].invert)

  def power(e: Int): NumericFuzzy[T] = mapFuncExp(powerT[T](e), x => frT.times(pow(x, e - 1), frT.fromInt(e)))(e == 0)

  // TODO add in the error due to the power function
  def power(e: Double): NumericFuzzy[Double] = mapFuncExp[Double](powerTx(e), x => powerTx(e - 1)(frT.toDouble(x)) * e)(e == 0)


  // TODO add in the error due to the exp function
  def exp: NumericFuzzy[Double] = mapFunc[Double](x => Fuzzy.exp(frT.toDouble(x)), x => Fuzzy.exp(frT.toDouble(x)))

  def compare(that: Fuzzy[T]): Int = {
    val z = minus(that)
    val p = z.p(frT.zero)
    if (p() > comparisonProbabilityThreshold) 0
    else math.signum(frT.toDouble(z())).toInt
  }

  def +[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = plus(uf)

  def -[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = minus(uf)

  def *[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = times(uf)

  def /[U: Fractional, V: Fractional](uf: Fuzzy[U]): NumericFuzzy[V] = div(uf)

  /**
    * Flip the parameters of the given function f
    *
    * @param f a function of (T, U) => V
    * @tparam U the U type
    * @tparam V the V type
    * @return the equivalent function of f but with parameters flipped so that it is of type (U, T) => V
    */
  protected def flip[U: Fractional, V: Fractional](f: (T, U) => V): (U, T) => V = Function.uncurried(FP.invert2(f.curried))

  /**
    * This class is for lazy map operations. However, at present, it is not used.
    *
    * @param f_      the value function
    * @param g_      the derivative function
    * @param isExact is Exact
    * @tparam U the underlying type of the result
    */
  class MappedFuzzy[U: Fractional](f_ : T => U, g_ : T => U, val isExact: Boolean) extends NumericFuzzy[U] {

    def getP(u: U): Option[Probability] = ???

    def map[V: Fractional](f: U => V, g: U => V): Fuzzy[V] = new MappedFuzzy[V](f_ andThen f, g_ andThen g, self.isExact)

    def apply(): U = f_(self())

    def compare(that: Fuzzy[U]): Int = ???

    def fuzziness: U = g_(self.fuzziness)

    def p(t1: U, t2: U)(implicit ev: Ordering[U]): Probability = ???
  }

}

object BaseNumericFuzzy {
  private def minusT[T: Fractional](t: T): T = implicitly[Fractional[T]].negate(t)

  private def plusTU[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].plus(toFractional[T, V](t), toFractional[U, V](u))

  private def timesTUV[T: Fractional, U: Fractional, V: Fractional](t: T, u: U): V = implicitly[Fractional[V]].times(toFractional[T, V](t), toFractional[U, V](u))

  private def timesDerivTUV[T: Fractional, U: Fractional, V: Fractional](tf: Fuzzy[T], uf: Fuzzy[U])(t: T, u: U): V = plusTU[T, U, V](implicitly[Fractional[T]].times(toFractional[U, T](uf()), t), implicitly[Fractional[U]].times(toFractional[T, U](tf()), u))

  private def powerT[T: Fractional](e: Int)(t: T): T = Fuzzy.pow(t, e)

  private def powerTx[T: Fractional](e: Double)(t: T): Double = Fuzzy.pow(implicitly[Fractional[T]].toDouble(t), e)

}

object Exact {
  def apply[T: Fractional](i: Int): Exact[T] = apply[T](implicitly[Numeric[T]].fromInt(i))
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

  def pow[N: Fractional](n: N, x: Int): N = {
    val f = implicitly[Fractional[N]]

    @tailrec def inner(r: N, k: Int): N = if (k == 0) r else inner(f.times(n, r), k - 1)

    val r = inner(f.fromInt(1), math.abs(x))
    if (x >= 0) r else invert(r)
  }

  def pow(x: Double, n: Double): Double = math.pow(x, n)

  def exp(y: Double): Double = math.exp(y)

  def invert[N: Fractional](n: N): N = div(fractional(1), n)

  def inRange[T: Numeric](min: T, t: T, max: T): Boolean = {
    val n = implicitly[Numeric[T]]
    n.compare(min, t) <= 0 && n.compare(t, max) <= 0
  }

  implicit object FuzzyIsFractionalDouble extends FuzzyIsFractional[Double] {
    def negate(xf: Fuzzy[Double]): Fuzzy[Double] = xf match {
      case Exact(x) => Exact(-x)
      case Bounded(x, b) => Bounded(-x, b)
    }

    def fromInt(x: Int): Fuzzy[Double] = Exact(x)
  }
}

object Bounded

case class FuzzyException(w: String) extends Exception(w)