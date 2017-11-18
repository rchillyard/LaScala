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
  * NOTE that the underlying type, N, is invariant.
  *
  * @author scalaprof
  * @param numerator   the numerator of this Rational
  * @param denominator the denominator of this Rational
  * @tparam N the (invariant) underlying integral type
  */
class Rational[N: FiniteIntegral](numerator: N, denominator: N) extends Ordered[Rational[N]] {

  type Builder = (N, N) => Rational[N]

  def n: N = numerator

  def d: N = denominator

  private val i = FiniteIntegral[N]

  // Pre-conditions:

  require(Rational.cf(d, 0) >= 0, s"Rational(#n,$d): denominator is negative")

  require(Rational.is(n, 0) && Rational.is(d, 0) || Rational.noCommonFactor(n, d), s"Rational($n,$d): arguments have common factor: ${Rational.absGcd(n, d)}")

  // Methods compare, equals and hashCode:

  /**
    * Method defined by Ordered
    *
    * @param other the Rational with which to compare this
    * @return
    */
  def compare(other: Rational[N]): Int = Rational.compare(this, other)

  /**
    * This should correspond to logic in hashCode (below).
    *
    * @param obj the object to be compared with this
    * @return true if the objects are considered equal
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case r@Rational(p, q) => isInfinity && r.isInfinity || n == p && q == d
    case _ => super.equals(obj)
  }

  /**
    * This should correspond to logic in equals (above).
    *
    * @return the hash code for this object.
    */
  override def hashCode(): Int = if (isInfinity) Int.MaxValue else n.hashCode() + d.hashCode() * 31

  // Operators

  /**
    * Operator + to add addend to this
    *
    * @param addend another Rational[N] -- the addend
    * @return the sum of this with addend
    */
  def +(addend: Rational[N]): Rational[N] = Rational.plus(this, addend)

  /**
    * Operator - to subtract subtrahend from this
    *
    * @param subtrahend the Rational object to be subtracted from this
    * @return the difference of this and subtrahend, i.e. this - subtrahend
    */
  def -(subtrahend: Rational[N]): Rational[N] = Rational.plus(this, subtrahend.negate)

  /**
    * Unary - operator -- to negate this
    *
    * @return the negative of this
    */
  def unary_- : Rational[N] = negate

  /**
    * Operator * to multiply this by multiplicand
    *
    * @param multiplicand the factor by which this is to be multiplied
    * @return this * multiplicand
    */
  def *(multiplicand: Rational[N]): Rational[N] = Rational.times(this, multiplicand)

  /**
    * Operator / to divide divisor into this
    *
    * @param divisor the divisor
    * @return the quotient of this divided by divisor
    */
  def /(divisor: Rational[N]): Rational[N] = this * divisor.invert

  /**
    * Operator &#94 to take this to the power of the given exponent
    *
    * @param exponent the power
    * @return this * this * ... * this (exponent times)
    */
  def ^(exponent: Int): Rational[N] = power(exponent)

  // Instance methods required by the operators defined above

  /**
    * Method to negate this
    *
    * @return this but with a negative numerator
    */
  def negate = new Rational(i.negate(n), d)

  /**
    * Method to invert this
    *
    * @return this but with the numerator and denominator swapped
    */
  def invert(implicit builder: Builder): Rational[N] = builder(d, n)

  /**
    * Raise this to the power of x. A tail-recursive method to perform x multiplications is used
    *
    * @param x the required exponent
    * @return the result of multiplying x by itself x times
    */
  def power(x: Int): Rational[N] = {
    @tailrec def inner(r: Rational[N], x: Int): Rational[N] = if (x == 0) r else inner(r * this, x - 1)

    inner(Rational.one, x)
  }

  // Methods to convert to other Numeric forms

  /**
    * Method to convert this Rational[N] to an Int
    *
    * For details, see Rational[N].toInt
    *
    * @return an Int corresponding to this Rational[N], if we're lucky
    */
  def toInt: Int = Rational.toInt(this)

  /**
    * Method to convert this Rational[N] to a Long
    *
    * For details, see Rational[N].toLong
    *
    * @return an Int corresponding to this Rational[N], if we're lucky
    */
  def toLong: Long = Rational.toLong(this)

  /**
    * Method to convert this Rational[N] to a Float
    *
    * invokes toDouble and converts that to Float
    *
    * @return a Float corresponding to this Rational[N]
    */
  def toFloat: Float = toDouble.toFloat

  /**
    * Method to convert this Rational[N] to a Double
    *
    * invokes toBigDecimal and converts that to Double
    *
    * @return a Double corresponding to this Rational[N]
    */
  def toDouble: Double = toBigDecimal.toDouble

  /**
    * Method to convert this Rational[N] to a BigDecimal
    *
    * @return a BigDecimal corresponding to this Rational[N]
    */
  def toBigDecimal: BigDecimal = d match {
    case 0 => throw new RationalException("value is infinite")
    case _ => BigDecimal(i.toBigInt(n)) / BigDecimal(i.toBigInt(d))
  }

  // Other methods appropriate to Rational[N]

  /**
    * The signum of this Rational[N]
    *
    * @return the signum of n (we assume that d is always positive)
    */
  def signum: Int = i.signum(n)

  def isNaN: Boolean = isZero && isInfinity

  def isWhole: Boolean = Rational.is(d, 1)

  def isZero: Boolean = Rational.is(n, 0)

  def isUnity: Boolean = Rational.is(n, 1) && isWhole

  def isInfinity: Boolean = Rational.is(d, 0)

  def floor: N = i.quot(n, d)

  def toRationalString = s"$n/$d"

  def isExactDouble: Boolean = Rational_Cross.isExactDouble(this)

  override def toString: String = if (isInfinity) "infinity" else if (isWhole) toLong.toString else if (Rational.cf(d, 100000) > 0 || isExactDouble) toDouble.toString else toRationalString

  /**
    * This method is provided to enable conversion of this Rational[N] into a Rational[M]
    *
    * @tparam M the underlying type of the result
    * @return a Rational[M] that is equal in value to this
    */
  def toRational[M: FiniteIntegral]: Rational[M] = Rational(Rational.convertToM(numerator)(implicitly[FiniteIntegral[N]], implicitly[FiniteIntegral[M]]), Rational.convertToM(denominator)(implicitly[FiniteIntegral[N]], implicitly[FiniteIntegral[M]]))
}

class RationalException(s: String, x: Exception = null) extends Exception(s, x)

object Rational {

  implicit def builder[N: FiniteIntegral](n: N, d: N): Rational[N] = new Rational[N](n, d)

  def unapply[N](r: Rational[N]) = Some(r.n, r.d)

  // Constants

  def zero[N: FiniteIntegral]: Rational[N] = apply(0)

  def infinity[N: FiniteIntegral]: Rational[N] = zero.invert

  def one[N: FiniteIntegral]: Rational[N] = apply(1)

  def ten[N: FiniteIntegral]: Rational[N] = apply(10)

  def half[N: FiniteIntegral]: Rational[N] = one / 2

  def NaN[N: FiniteIntegral] = new Rational(0, 0)

  def max[N: FiniteIntegral](r1: Rational[N], r2: Rational[N]): Rational[N] = if (r1 >= r2) r1 else r2

  def min[N: FiniteIntegral](r1: Rational[N], r2: Rational[N]): Rational[N] = if (r1 <= r2) r1 else r2

  def convertToM[N: FiniteIntegral, M: FiniteIntegral](n: N): M = implicitly[FiniteIntegral[M]].fromBigInt(implicitly[FiniteIntegral[N]].toBigInt(n))

  // Apply methods

  def apply[N: FiniteIntegral](n: N, d: N): Rational[N] = normalize(n, d)

  /**
    * Apply method to form a Rational from a FiniteIntegral
    *
    * @param x the FiniteIntegral
    * @return the Rational corresponding to x
    */
  def apply[N: FiniteIntegral](x: N): Rational[N] = new Rational[N](x, FiniteIntegral[N].one)

  /**
    * Apply method to form a Rational from a BigDecimal
    *
    * @param x the BigDecimal
    * @return the Rational corresponding to x
    */
  def apply[N: FiniteIntegral](x: BigDecimal): Rational[N] = {
    val i = FiniteIntegral[N]
    if (x.scale >= 0) {
      val e = BigDecimal(10).pow(x.scale)
      Rational.normalize(i.fromLong((x * e).toLongExact), i.fromLong(e.longValue))
    }
    else
      apply(i.fromBigInt(x.toBigInt))
  }

  /**
    * Apply method to form a Rational from a Double
    *
    * @param x the BigDecimal
    * @return the Rational corresponding to x
    */
  def apply[N: FiniteIntegral](x: Double): Rational[N] = apply(BigDecimal.valueOf(x))

  def fromFractional[N: FiniteIntegral, M: Fractional](x: M): Rational[N] = x match {
    case r@Rational(_, _) => r.toRational
    case x: Int => Rational[N](x)
    case x: Long => Rational[N](x)
    case x: Double => Rational[N](x)
    case x: BigDecimal => Rational[N](x)
    case _ => throw new RationalException(s"cannot apply $x (unsupported)")
  }

  /**
    * Apply method to form a Rational from a String
    *
    * @param x the String
    * @return the Rational corresponding to x
    */
  def apply[N: FiniteIntegral](x: String): Rational[N] = {
    val i = FiniteIntegral[N]
    val rRat = """^\s*(-?\d+)\s*(\/\s*(-?\d+)\s*)?$""".r
    val rDec = """(?i)^(-?)(\d|(\d+,?\d+))*(\.\d+)?(E\d+)?$""".r
    x match {
      case rRat(n) => apply(i.fromString(n))
      // XXX I don't understand why we need this line -- but it IS necessary -- the regex looks good but apparently isn't
      case rRat(n, _, null) => apply(i.fromString(n))
      // XXX we need to watch out for NotNumber exceptions
      case rRat(n, _, d) => Rational.normalize[N](i.fromString(n), i.fromString(d))
      case rDec(s, w, _, f, null) => Rational(BigDecimal(s + w + f))
      case rDec(s, w, _, f, e) => Rational(BigDecimal(s + w + f + e))
      case _ => throw new RationalException(s"invalid rational expression: $x")
    }
  }

  /**
    * Implicit class RationalHelper to support the strings of form r"22/7", etc.
    *
    * @param sc the StringContext
    */
  implicit class RationalHelper(val sc: StringContext) extends AnyVal {
    def r(args: Any*): Rational[Long] = {
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

  // Object methods required by the operators defined for Rational[N]

  /**
    * Method to add two Rational[N] objects together.
    * The order of the parameters is immaterial.
    *
    * @param x one of the Rational[N]s
    * @param y the other Rational[N]
    * @return a new Rational[N] which is the sum of x and y
    */
  def plus[N: FiniteIntegral](x: Rational[N], y: Rational[N])(implicit builder: (N, N) => Rational[N]): Rational[N] = {
    val i = FiniteIntegral[N]
    normalize(i.plus(i.times(x.n, y.d), i.times(x.d, y.n)), i.times(x.d, y.d))
  }

  /**
    * Method to multiply two Rational[N] objects together.
    * The order of the parameters is immaterial.
    *
    * @param x one of the Rational[N]s
    * @param y the other Rational[N]
    * @return a new Rational[N] which is the product of x and y
    */
  def times[N: FiniteIntegral](x: Rational[N], y: Rational[N])(implicit builder: (N, N) => Rational[N]): Rational[N] = {
    val i = FiniteIntegral[N]
    normalize(i.times(x.n, y.n), i.times(x.d, y.d))
  }

  // Method required by the Ordered.compare method (above)

  /**
    * Method to compare two Rational[N] numbers
    *
    * @param x the first Rational[N]
    * @param y the second Rational[N]
    * @return (x-y).signum
    */
  def compare[N: FiniteIntegral](x: Rational[N], y: Rational[N]): Int = if (x.isInfinity && y.isInfinity) 0 else (x - y).signum

  /**
    * Method to convert a Rational (x) into an Int
    *
    * @param x the Rational to convert
    * @return if x is whole and if x fits into the range of an Int, then the corresponding Int is returned;
    *         otherwise, we throw a FiniteIntegralException
    */
  def toInt[N: FiniteIntegral](x: Rational[N]): Int = narrowWhole(x)(FiniteIntegral[N].toInt)

  /**
    * Method to convert a Rational (x) into a Long
    *
    * @param x the Rational to convert
    * @return if x is whole, then the corresponding Long is returned;
    *         otherwise, we throw a FiniteIntegralException
    */
  def toLong[N: FiniteIntegral](x: Rational[N]): Long = narrowWhole(x)(FiniteIntegral[N].toLong)

  /**
    * Method to convert a Rational (x) into a BigInt
    *
    * @param x the Rational to convert
    * @return if x is whole, then the corresponding BigInt is returned.
    */
  def toBigInt[N: FiniteIntegral](x: Rational[N]): BigInt = narrowWhole(x)(FiniteIntegral[N].toBigInt)

  /**
    * Method to form normalized Rational[N] from the given numerator and denominator
    *
    * @param n the numerator
    * @param d the denominator
    * @return a Rational[N] corresponding to n/d where n and d have no common factors
    */
  def normalize[N: FiniteIntegral](n: N, d: N)(implicit builder: (N, N) => Rational[N]): Rational[N] = {
    val i = FiniteIntegral[N]
    val g = absGcd(n, d)
    g match {
      case 0 =>
        builder(i.zero, i.zero)
      case _ =>
        val f = i.fromInt(i.signum(d))
        val numerator = i.quot(i.times(n, f), g)
        val denominator = i.quot(i.times(d, f), g)
        // If the numerator or denominator gets too big for N, we will just throw an exception.
        // We can't do anything else here to handle it.
        // The caller might possibly be able to try again with a different (wider) N type.
        builder(numerator, denominator)
    }
  }

  /**
    * Method to determine the greatest common divisor of a and b
    *
    * @param a the first non-negative FiniteIntegral value
    * @param b the second non-negative FiniteIntegral value
    * @return their greatest common divisor
    */
  @tailrec private def gcd[N: FiniteIntegral](a: N, b: N): N = if (b == 0) a else gcd(b, FiniteIntegral[N].rem(a, b))

  /**
    * Method to determine the greatest common divisor of a and b, where a and b may be signed
    *
    * @param a the first FiniteIntegral value
    * @param b the second FiniteIntegral value
    * @return their greatest common divisor
    */
  private def absGcd[N: FiniteIntegral](a: N, b: N): N = gcd(integralAbs(a), integralAbs(b))

  /**
    * Method to ensure that a and b have no common factor
    *
    * @param a a value
    * @param b another value
    * @tparam N the underlying type
    * @return true if the greatest common divisor of a and b is 1
    */
  private def noCommonFactor[N: FiniteIntegral](a: N, b: N) = absGcd(a, b) == 1


  /**
    * Method to get the absolute value of a Long
    *
    * @param a the Long value
    * @return the absolute value of a
    */
  private def integralAbs[N: FiniteIntegral](a: N) = FiniteIntegral[N].abs(a)

  // Utilities

  private def cf[N: FiniteIntegral](x: N, y: N): Int = FiniteIntegral[N].compare(x, y)

  private def cf[N: FiniteIntegral](x: N, y: Int): Int = cf(x, FiniteIntegral[N].fromInt(y))

  private def is[N: FiniteIntegral](x: N, y: Int): Boolean = cf(x, y) == 0

  private def narrowWhole[N: FiniteIntegral, M](x: Rational[N])(f: N => M): M = if (x.isWhole) f(x.n) else throw new RationalException(s"$x is not Whole")

  /**
    * Implicit conversion method from Int=>Rational.
    * This is used for example when comparing a Rational value with an Int
    *
    * @param x an Int
    * @return a Rational with the same value as x
    */
  implicit def intToRational[N: FiniteIntegral](x: Int): Rational[N] = apply(FiniteIntegral[N].fromInt(x))

  /**
    * Implicit conversion method from Long=>Rational[N].
    * This is used for example when comparing a Rational[N] value with an Long
    *
    * @param x an Long
    * @return a Rational[N] with the same value as x
    */
  implicit def longToRational[N: FiniteIntegral](x: Long): Rational[N] = apply(FiniteIntegral[N].fromLong(x))

  /**
    * Implicit conversion method from Double=>Rational[N].
    * This is used for example when comparing a Rational[N] value with an Double
    *
    * @param x a Double
    * @return a Rational[N] with the same value as x, or at least as close as possible
    */
  implicit def doubleToRational[N: FiniteIntegral](x: Double): Rational[N] = Rational(x)

  /**
    * Trait which enables Rational[N] to be used in type classes where the context type is a Fractional (or Numeric or Ordering).
    */
  trait RationalIsFractional[N] extends Fractional[Rational[N]] {

    // Members declared in scala.math.Numeric -- see super methods for scaladoc

    def plus(x: Rational[N], y: Rational[N]): Rational[N] = x + y

    def minus(x: Rational[N], y: Rational[N]): Rational[N] = x - y

    def times(x: Rational[N], y: Rational[N]): Rational[N] = x * y

    def toInt(x: Rational[N]): Int = x.toInt

    def toLong(x: Rational[N]): Long = x.toLong

    def toFloat(x: Rational[N]): Float = x.toFloat

    def toDouble(x: Rational[N]): Double = x.toDouble

    //Members declared in scala.math.Fractional
    def div(x: Rational[N], y: Rational[N]): Rational[N] = x / y

    // Members declared in scala.math.Ordering
    def compare(x: Rational[N], y: Rational[N]): Int = x.compare(y)
  }

  class RationalIsFractionalAndFiniteNumeric[N: FiniteIntegral] extends RationalIsFractional[N] {

    private val i = FiniteIntegral[N]

    def fromInt(x: Int): Rational[N] = Rational[N](i.fromInt(x))

    override def zero: Rational[N] = Rational(i.zero)

    override def one: Rational[N] = Rational(i.one)

    override def abs(x: Rational[N]): Rational[N] = Rational(i.abs(x.n), x.d)

    override def signum(x: Rational[N]): Int = i.signum(x.n)

    def negate(x: Rational[N]): Rational[N] = Rational(i.negate(x.n), x.d)
  }

  /**
    * Implicit objects which extend RationalIsFractional.
    * BTW, it's OK for them to share the same name.
    */
  implicit object RationalIsFractionalAndFiniteNumericInt extends RationalIsFractionalAndFiniteNumeric[Int]

  implicit object RationalIsFractionalAndFiniteNumericLong extends RationalIsFractionalAndFiniteNumeric[Long]

  implicit object RationalIsFractionalAndFiniteNumericBigInt extends RationalIsFractionalAndFiniteNumeric[BigInt]

}
