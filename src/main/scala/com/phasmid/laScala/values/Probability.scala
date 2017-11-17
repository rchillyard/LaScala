/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

/**
  * This trait defines the operations of a Probability
  */
trait Probability extends (() => Double) {
  /**
    * Method to form the disjunction (sum) of this and another probability.
    * Note that the two probability must of independent events for this to yield the correct answer
    *
    * @param p the other Probability
    * @return the disjunction of this and p
    */
  def |(p: Probability): Probability

  /**
    * Method to form the conjunction (product) of this and another probability.
    *
    * @param p the other Probability
    * @return the conjunction of this and p
    */
  def &(p: Probability): Probability

  /**
    * Method to form the complement of this probability
    *
    * @return the probability whose numerical value is 1 - this
    */
  def ! : Probability

  /**
    * Method to factor this probability by a scalar value
    *
    * @return this probability factored by x
    */
  def *[N: Fractional](x: N): Probability
}

/**
  * Probability which is based on a Rational[N]
  *
  * NOTE that for the | and & methods, we need to convert q into Rational[N].
  * This is because the unapply method for RationalProbability returns a Rational[Any]
  *
  * @param p the probability as a Rational[N]
  * @tparam N the underlying type of Rational[N], with implicit evidence FiniteIntegral[N]
  */
case class RationalProbability[N: FiniteIntegral](p: Rational[N]) extends Probability {
  require(p >= Rational.zero && p <= Rational.one, s"p $p is not in range 0..1")

  override def apply(): Double = p.toDouble

  def |(o: Probability): Probability = o match {
    case RationalProbability(q) => RationalProbability(Rational.min[N](p + q.toRational, Rational.one[N]))
    case _ => o | this
  }

  def &(o: Probability): Probability = o match {
    case RationalProbability(q) => RationalProbability[N](p * q.toRational)
    case _ => o | this
  }

  def *(r: Rational[N]): Probability = RationalProbability(p * r)

  def ! = RationalProbability(-p + Rational.one)

  override def hashCode(): Int = ().hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case RationalProbability(q) => p.equals(q)
    case q: Probability => apply() == q()
    case _ => false
  }

  def *[M: Fractional](x: M): Probability = RationalProbability(Rational.fromFractional[N, M](x) * p)
}

/**
  * Probability which is based on a Double
  *
  * @param p the probability as a Double
  */
case class GenericProbability(p: Double) extends Probability {
  require(p >= 0 && p <= 1, s"p $p is not in range 0..1")

  override def apply(): Double = p

  def |(o: Probability) = GenericProbability(this () + o())

  def &(o: Probability) = GenericProbability(this () * o())

  def ! = GenericProbability(1 - p)

  override def hashCode(): Int = ().hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case GenericProbability(q) => p.equals(q)
    case q: Probability => apply() == q()
    case _ => false
  }

  def *[M: Fractional](x: M): Probability = GenericProbability(implicitly[Fractional[M]].toDouble(x) * p)
}

object Probability {
  def apply(fav: Int, total: Int): Probability = RationalProbability[Int](Rational[Int](fav, total))

  def apply[N: FiniteIntegral](p: Rational[N]): Probability = RationalProbability(p)

  def apply(p: Double): Probability = GenericProbability(normalize(p))

  def normalize(p: Double): Double = math.min(math.max(p, 0), 1)

  val Certain = Probability(1, 1)
  val Impossible = Probability(0, 1)
}

object RationalProbability {
}

object GenericProbability {
}