package com.phasmid.laScala.values

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.BigDecimal


/**
  * A simple Rational type for developers not depending on Spire or something like that.
  * This grew out of a class exercise so is not the last word in sophistication.
  *
  * NOTE that it is not expected that applications will directly reference this case class constructor.
  * If they do, they must ensure that d is positive and that there are no common factors for n and d
  *
  * @author scalaprof
  */
case class Rational(n: Long, d: Long) extends Ordered[Rational] {

  // Pre-conditions
  require(d>=0, s"Rational(#n,$d): denominator is negative")

  require(n==0 && d==0 || Rational.gcd(math.abs(n), math.abs(d)) == 1, s"Rational($n,$d): arguments have common factor: ${Rational.gcd(n, d)}")

  /**
    * Method defined by Ordered
    * @param other the Rational with which to compare this
    * @return
    */
  def compare(other: Rational): Int = Rational.compare(this, other)

  /**
    * This should correspond to logic in hashCode (below).
    *
    * @param obj the object to be compared with this
    * @return true if the objects are considered equal
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case Rational(p,q) => q==0 && d==0 || n==p && q==d
    case _ => super.equals(obj)
  }

  /**
    * This should correspond to logic in equals (above).
    *
    * @return the hash code for this object.
    */
  override def hashCode(): Int = super.hashCode()

  // Operators

  /**
    * Operator + to add addend to this
    * @param addend another Rational -- the addend
    * @return the sum of this with addend
    */
  def +(addend: Rational): Rational = Rational.plus(this, addend)

  /**
    * Operator - to subtract subtrahend from this
    * @param subtrahend
    * @return the difference of this and subtrahend, i.e. this - subtrahend
    */
  def -(subtrahend: Rational): Rational = Rational.plus(this, subtrahend.negate)

  /**
    * Unary - operator -- to negate this
    * @return the negative of this
    */
  def unary_- : Rational = negate

  /**
    * Operator * to multiply this by multiplicand
    * @param multiplicand the factor by which this is to be multiplied
    * @return this * multiplicand
    */
  def *(multiplicand: Rational): Rational = Rational.times(this, multiplicand)

  /**
    * Operator / to divide divisor into this
    * @param divisor
    * @return
    */
  def /(divisor: Rational): Rational = this * divisor.invert

  /**
    * Operator '^' to take this to the power of the given exponent
    * @param exponent
    * @return this * this * ... * this (exponent times)
    */
  def ^(exponent: Int): Rational = power(exponent)

  // Instance methods required by the operators defined above

  /**
    * Method to negate this
    * @return this but with a negative numerator
    */
  def negate: Rational = Rational(-n, d)

  /**
    * Method to invert this
    * @return this but with the numerator and denominator swapped
    */
  def invert = Rational(d, n)

  /**
    * Raise this to the power of x. A tail-recursive method to perform x multiplications is used
    * @param x the required exponent
    * @return the result of multiplying x by itself x times
    */
  def power(x: Int): Rational = {
    @tailrec def inner(r: Rational, x: Int): Rational = if (x == 0) r else inner(r * this, x - 1)
    inner(Rational.one, x)
  }

  // Methods to convert to other Numeric forms

  /**
    * Method to convert this Rational to an Int
    *
    * For details, see Rational.toInt
    *
    * @return an Int corresponding to this Rational, if we're lucky
    */
  def toInt: Int = Rational.toInt(this)

  /**
    * Method to convert this Rational to a Long
    *
    * For details, see Rational.toLong
    *
    * @return an Int corresponding to this Rational, if we're lucky
    */
  def toLong: Long = Rational.toLong(this)

  def toFloat: Float = toDouble.toFloat

  def toDouble: Double = toBigDecimal.toDouble

  def toBigDecimal: BigDecimal = d match {
    case 0 => throw new RationalException("value is infinite")
    case _ => BigDecimal(n) / d
  }

  // Other methods appropriate to Rational

  /**
    * The signum of this Rational
    * @return the signum of n (we assume that d is always positive)
    */
  def signum: Int = math.signum(n).toInt

  def isNaN: Boolean = isZero && isInfinity

  def isWhole: Boolean = d == 1L

  def isZero: Boolean = n == 0L

  def isUnity: Boolean = n == 1L && isWhole

  def isInfinity: Boolean = d == 0L

  def floor: Long = n / d

  def toRationalString = s"$n/$d"

  def isExactDouble: Boolean = Rational_Cross.isExactDouble(this)

  override def toString: String = if (isInfinity) "infinity" else if (isWhole) toLong.toString else if (d > 100000L || isExactDouble) toDouble.toString else toRationalString
}

class RationalException(s: String, x: Exception = null) extends Exception(s, x)

object Rational {

  // Object methods required by the operators defined for Rational

  /**
    * Method to add two Rational objects together.
    * The order of the parameters is immaterial.
    *
    * @param x one of the Rationals
    * @param y the other Rational
    * @return a new Rational which is the sum of x and y
    */
  def plus(x: Rational, y: Rational): Rational = normalize(x.n * y.d + x.d * y.n, x.d * y.d)

  /**
    * Method to multiply two Rational objects together.
    * The order of the parameters is immaterial.
    *
    * @param x one of the Rationals
    * @param y the other Rational
    * @return a new Rational which is the product of x and y
    */
  def times(x: Rational, y: Rational): Rational = normalize(x.n * y.n, x.d * y.d)

  // Method required by the Ordered.compare method (above)

  /**
    * Method to compare two Rational numbers
    * @param x the first Rational
    * @param y the second Rational
    * @return (x-y).signum
    */
  def compare(x: Rational, y: Rational): Int = if (x.isInfinity && y.isInfinity) 0 else (x-y).signum

  /**
    * Method to convert a Rational (x) into an Int
    * @param x the Rational to convert
    * @return if x is whole and if x fits into the range of an Int, then the corresponding Int is returned;
    *         otherwise, we throw a RationalException
    */
  def toInt(x: Rational): Int = {
    val l = toLong(x)
    if (Rational.longAbs(l) < Int.MaxValue) l.toInt else throw new RationalException(s"$x is too big for Int")
  }

  /**
    * Method to convert a Rational (x) into a Long
    * @param x the Rational to convert
    * @return if x is whole, then the corresponding Long is returned;
    *         otherwise, we throw a RationalException
    */
  def toLong(x: Rational): Long = if (x.isWhole) x.n else throw new RationalException(s"$x is not Whole")

  /**
    * Implicit class RationalHelper to support the strings of form r"22/7", etc.
    * @param sc the StringContext
    */
  implicit class RationalHelper(val sc: StringContext) extends AnyVal {
    def r(args: Any*): Rational = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuffer()
      while (strings.hasNext) {
        val s = strings.next
        if (s.isEmpty) {
          if (expressions.hasNext)
            sb.append(expressions.next)
          else
            throw new RationalException("r: logic error: missing expression")
        }
        else
          sb.append(s)
      }
      if (expressions.hasNext)
        throw new RationalException(s"r: ignored: ${expressions.next}")
      else
        Rational(sb.toString)
    }
  }

  // Constants

  val zero = Rational(0)
  val infinity: Rational = zero.invert
  val one = Rational(1)
  val ten = Rational(10)
  val half: Rational = Rational(1,2)
  val NaN = new Rational(0,0)

  // Apply methods

  /**
    * Apply method to form a Rational from an Int
    * @param x the Int
    * @return the Rational corresponding to x
    */
  def apply(x: Int): Rational = apply(x.toLong)

  /**
    * Apply method to form a Rational from a Long
    * @param x the Long
    * @return the Rational corresponding to x
    */
  def apply(x: Long): Rational = new Rational(x, 1)

  /**
    * Apply method to form a Rational from a BigDecimal
    * @param x the BigDecimal
    * @return the Rational corresponding to x
    */
  def apply(x: BigDecimal): Rational = if (x.scale >= 0) {
    val e = BigDecimal.apply(10).pow(x.scale)
    normalize((x * e).toLongExact, e.longValue)
  }
  else
    Rational(x.toLongExact)

  /**
    * Apply method to form a Rational from a String
    * @param x the String
    * @return the Rational corresponding to x
    */
  def apply(x: String): Rational = {
    val rRat = """^\s*(-?\d+)\s*(\/\s*(-?\d+)\s*)?$""".r
    val rDec = """(?i)^(-?)(\d|(\d+,?\d+))*(\.\d+)?(E\d+)?$""".r
    x match {
      case rRat(n) => Rational(n.toLong)
      // XXX I don't understand why we need this line -- but it IS necessary -- the regex looks good but apparently isn't
      case rRat(n, _, null) => Rational(n.toLong)
      case rRat(n, _, d) => normalize(n.toLong, d.toLong)
      case rDec(s, w, _, f, null) => Rational(BigDecimal.apply(s + w + f))
      case rDec(s, w, _, f, e) => Rational(BigDecimal.apply(s + w + f + e))
      case _ => throw new RationalException(s"invalid rational expression: $x")
    }
  }

  /**
    * Method to form normalized Rational from the given numerator and denominator
    * @param n the numerator
    * @param d the denominator
    * @return a Rational corresponding to n/d where n and d have no common factors
    */
  def normalize(n: Long, d: Long): Rational = {
    val g = gcd(math.abs(n), math.abs(d))
    g match {
      case 0 =>
        Rational.NaN
      case _ =>
        val f = math.signum(d)
        apply(f * n / g, f * d / g)
    }
  }

  /**
    * Method to determine the greatest common divisor of a and b
    * @param a the first Long value
    * @param b the second Long value
    * @return their greatest common divisor
    */
  @tailrec private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  /**
    * Method to get the absolute value of a Long
    * @param a the Long value
    * @return the absolute value of a
    */
  private def longAbs(a: Long) = if (a < 0) -a else a

  /**
    * Implicit conversion method from Int=>Rational.
    * This is used for example when comparing a Rational value with an Int
    * @param x an Int
    * @return a Rational with the same value as x
    */
  implicit def intToRational(x: Int): Rational = Rational(x)

  /**
    * Implicit conversion method from Long=>Rational.
    * This is used for example when comparing a Rational value with an Long
    * @param x an Long
    * @return a Rational with the same value as x
    */
  implicit def longToRational(x: Long): Rational = Rational(x)

  /**
    * Implicit conversion method from Double=>Rational.
    * This is used for example when comparing a Rational value with an Double
    * @param x a Double
    * @return a Rational with the same value as x, or at least as close as possible
    */
  implicit def doubleToRational(x: Double): Rational = Rational(x)

  /**
    * Trait which enables Rational to be used in type classes where the context type is a Fractional (or Numeric or Ordering).
    */
  trait RationalIsFractional extends Fractional[Rational] {

    // Members declared in scala.math.Numeric -- see super methods for scaladoc

    def plus(x: Rational, y: Rational): Rational = x + y

    def minus(x: Rational, y: Rational): Rational = x - y

    def times(x: Rational, y: Rational): Rational = x * y

    def negate(x: Rational): Rational = Rational(-x.n, x.d)

    def fromInt(x: Int): Rational = Rational(x)

    def toInt(x: Rational): Int = x.toInt

    def toLong(x: Rational): Long = x.toLong

    def toFloat(x: Rational): Float = x.toFloat

    def toDouble(x: Rational): Double = x.toDouble

    override def zero: Rational = Rational.zero

    override def one: Rational = Rational.one

    override def abs(x: Rational): Rational = x.abs()

    override def signum(x: Rational): Int = x.signum

    //Members declared in scala.math.Fractional
    def div(x: Rational, y: Rational): Rational = x / y

    // Members declared in scala.math.Ordering
    def compare(x: Rational, y: Rational): Int = x.compare(y)
  }

  /**
    * Implicit object which extends RationalIsFractional.
    * BTW, it's OK for them to share the same name.
    */
  implicit object RationalIsFractional extends RationalIsFractional

}
