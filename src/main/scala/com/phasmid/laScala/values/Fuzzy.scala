/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

/**
  * Typeclass Fuzzy which adds fuzzy behavior to any type.
  *
  * The method apply() will provide the nominal (i.e. most probable) value
  *
  * @tparam T the type to which we add the fuzzy behavior
  */
trait Fuzzy[T] extends (() => T) {

  /**
    * Method to return EITHER the Probability of the nominal value of this Fuzzy being the same as t.
    * OR (if T is continuous Numeric, such as Double) the probability density function at t.
    *
    * @param t the value of t for which we wish to evaluate the probability of it matching the nominal value.
    * @return either a finite Probability or the value of the probability density function at t.
    */
  def p(t: T): Probability
}

case class Bounded[T: Fractional](t: T, bound: T) extends Fuzzy[T] {
  override def apply(): T = t

  import Bounded._
  import Fuzzy._
  import Probability._

  private val min = minus(t, bound)
  private val max = plus(t, bound)

  def p(t: T): Probability = if (inRange(min, t, max)) Certain * reciprocal(times(bound, 2))
  else Impossible
}

object Fuzzy {

  def inRange[T: Numeric](x: T, y: T, z: T): Boolean = {
    val n = implicitly[Numeric[T]]
    n.compare(x, y) <= 0 && n.compare(y, z) <= 0
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

  def reciprocal[N: Fractional](n: N): N = div(fractional(1), n)
}