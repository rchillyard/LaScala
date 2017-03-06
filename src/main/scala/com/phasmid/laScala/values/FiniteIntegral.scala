package com.phasmid.laScala.values

import scala.math.Numeric.{BigIntIsIntegral, IntIsIntegral, LongIsIntegral}

/**
  * This trait represents the concept of an integral value defined over a finite range.
  * Real-life examples include Long, Integer, Short, etc.
  *
  * Created by scalaprof on 1/3/17.
  */
trait FiniteIntegral[N] extends Integral[N] {
  /**
    * Method to determine of n is in the range of the underlying type N
    *
    * @param n a BigInt
    * @return true if n can be narrowed into an N
    */
  def inRange(n: BigInt): Boolean

  /**
    * widen the value n into a BigInt
    *
    * @param n the value
    * @return a BigInt corresponding to n
    */
  def toBigInt(n: N): BigInt

  /**
    * narrow the value n to the underlying type N
    *
    * @param n a BigInt
    * @return the N corresponding to n else...
    * @throws FiniteIntegralException if s is out of range of N
    */
  def fromBigInt(n: BigInt): N

  /**
    * narrow/widen the value n to the underlying type N
    *
    * @param n a Long
    * @return the N corresponding to n else...
    * @throws FiniteIntegralException if s is out of range of N
    */
  def fromLong(n: Long): N = fromBigInt(BigInt(n))

  /**
    * parse the String s to the underlying type N
    *
    * @param s a String
    * @return the N corresponding to n else...
    * @throws FiniteIntegralException if s is out of range of N
    */
  def fromString(s: String): N = fromBigInt(BigInt(s))
}

/**
  * This trait defines the behavior required by Numeric[N]
  *
  * @tparam N the underlying integral type, such as Int, Long, BigInt
  */
trait FiniteIntegralNumeric[N] extends FiniteIntegral[N] {

  /**
    * Override plus so that an exception will be generated if the result is too big to be represented by N
    *
    * @param x an N
    * @param y another N
    * @return the sum of x and y or an exception
    * @throws FiniteIntegralException if x+y is out of range of N
    */
  override def plus(x: N, y: N): N = fromBigInt(toBigInt(x) + toBigInt(y))

  /**
    * Override times so that an exception will be generated if the result is too big to be represented by N
    *
    * @param x an N
    * @param y another N
    * @return the product of x and y or an exception
    * @throws FiniteIntegralException if x*y is out of range of N
    */
  override def times(x: N, y: N): N = fromBigInt(toBigInt(x) * toBigInt(y))
}

object FiniteIntegral {

  def apply[N: FiniteIntegral]: FiniteIntegral[N] = implicitly[FiniteIntegral[N]]

  /**
    * Trait which enables Integral objects to be used in type classes where the
    */
  trait IntIsFiniteIntegral extends IntIsIntegral with FiniteIntegralNumeric[Int] with Ordering.IntOrdering {
    def toBigInt(n: Int): BigInt = BigInt(n)

    def fromBigInt(n: BigInt): Int = if (inRange(n)) n.toInt else throw FiniteIntegralException(n, Int.getClass)

    def inRange(n: BigInt): Boolean = n.compare(Int.MinValue) > 0 && n.compare(Int.MaxValue) < 0
  }

  /**
    * Implicit object which extends IntIsFiniteIntegral.
    */
  implicit object IntIsFiniteIntegral extends IntIsFiniteIntegral

  /**
    * Trait which enables Integral objects to be used in type classes where the
    */
  trait LongIsFiniteIntegral extends LongIsIntegral with FiniteIntegralNumeric[Long] with Ordering.LongOrdering {
    override def toInt(x: Long): Int = IntIsFiniteIntegral.fromBigInt(x)

    def toBigInt(n: Long): BigInt = BigInt(n)

    def fromBigInt(n: BigInt): Long = if (inRange(n)) n.toLong else throw FiniteIntegralException(n, Long.getClass)

    def inRange(n: BigInt): Boolean = n.compare(Long.MinValue) > 0 && n.compare(Long.MaxValue) < 0
  }

  /**
    * Implicit object which extends LongIsFiniteIntegral.
    */
  implicit object LongIsFiniteIntegral extends LongIsFiniteIntegral

  /**
    * Trait which enables Integral objects to be used in type classes where the
    */
  trait BigIntIsFiniteIntegral extends BigIntIsIntegral with FiniteIntegral[BigInt] with Ordering.BigIntOrdering {
    def fromBigInt(n: BigInt): BigInt = n

    override def toInt(x: BigInt): Int = IntIsFiniteIntegral.fromBigInt(x)

    override def toLong(x: BigInt): Long = LongIsFiniteIntegral.fromBigInt(x)

    def toBigInt(n: BigInt): BigInt = n

    def inRange(n: BigInt): Boolean = true
  }

  /**
    * Implicit object which extends BigIntIsFiniteIntegral.
    */
  implicit object BigIntIsFiniteIntegral extends BigIntIsFiniteIntegral

}

import scala.language.existentials
case class FiniteIntegralException(r: BigInt, c: Class[_]) extends Exception(s"$r is out of range for $c")
