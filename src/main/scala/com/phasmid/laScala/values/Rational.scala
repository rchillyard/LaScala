package com.phasmid.laScala.values

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.BigDecimal

/**
  * A simple Rational type for developers not depending on Spire or something like that.
  * This grew out of a class exercise so is not the last word in sophistication.
  *
  * @author scalaprof
  */
case class Rational(n: Long, d: Long) {

  // Pre-conditions
  require(n==0 && d==0 || Rational.gcd(math.abs(n), math.abs(d)) == 1, s"Rational($n,$d): arguments have common factor: ${Rational.gcd(n, d)}")

  override def equals(obj: scala.Any): Boolean = obj match {
    case Rational(p,q) => q==0 && d==0 || n==p && q==d
    case _ => super.equals(obj)
  }

  /**
    * This should correspond to logic in equals
    *
    * @return
    */
  override def hashCode(): Int = super.hashCode()

  // Operators
  def +(that: Rational): Rational = plus(this, that)

  def +(that: Long): Rational = this + Rational(that)

  def -(that: Rational): Rational = minus(this, that)

  def -(that: Long): Rational = this - Rational(that)

  def unary_- : Rational = negate(this)

  def *(that: Rational): Rational = times(this, that)

  def *(that: Long): Rational = this * Rational(that)

  def /(that: Rational): Rational = this * that.invert

  def /(that: Long): Rational = this / Rational(that)

  def ^(that: Int): Rational = power(that)

  // Members declared in scala.math.Numeric
  def fromInt(x: Int): Rational = Rational.apply(x)

  def minus(x: Rational, y: Rational): Rational = plus(x, negate(y))

  def negate(x: Rational): Rational = Rational(-x.n, x.d)

  def plus(x: Rational, y: Rational): Rational = Rational.normalize(x.n * y.d + x.d * y.n, x.d * y.d)

  def times(x: Rational, y: Rational): Rational = Rational.normalize(x.n * y.n, x.d * y.d)

  def toDouble(x: Rational): Double = x.n * 1.0d / x.d

  def toFloat(x: Rational): Float = toDouble(x).toFloat

  def toInt(x: Rational): Int = {
    val l = toLong(x)
    if (Rational.longAbs(l) < Int.MaxValue) l.toInt else throw new RationalException(s"$x is too big for Int")
  }

  def toLong(x: Rational): Long = if (x.isWhole) x.n else throw new RationalException(s"$x is not Whole")

  //Members declared in scala.math.Fractional
  def div(x: Rational, y: Rational): Rational = x / y

  // Members declared in scala.math.Ordering
  def compare(x: Rational, y: Rational): Int = if (x.isInfinity && y.isInfinity) 0 else minus(x, y).signum

  // Other methods appropriate to Rational
  def signum: Int = math.signum(n).toInt

  def invert = Rational(d, n)

  def isWhole: Boolean = d == 1L

  def isZero: Boolean = n == 0L

  def isUnity: Boolean = n == 1L && isWhole

  def isInfinity: Boolean = d == 0L

  def toInt: Int = toInt(this)

  def toLong: Long = toLong(this)

  def floor: Long = n / d

  def toFloat: Float = toFloat(this)

  def toDouble: Double = toDouble(this)

  def power(x: Int): Rational = {
    @tailrec def inner(r: Rational, x: Int): Rational = if (x == 0) r else inner(r * this, x - 1)
    inner(Rational.one, x)
  }

  def toBigDecimal: BigDecimal = d match {
    case 0 => throw new RationalException("value is infinite")
    case _ => BigDecimal(n) / d
  }

  def compare(other: Rational): Int = compare(this, other)

  def toRationalString = s"$n/$d"

  def isExactDouble: Boolean = Rational_Cross.isExactDouble(this)

  override def toString: String = if (isInfinity) "infinity" else if (isWhole) toLong.toString else if (d > 100000L || isExactDouble) toDouble.toString else toRationalString
}

class RationalException(s: String) extends Exception(s)

object Rational {

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

  val zero = Rational(0)
  val infinity: Rational = zero.invert
  val one = Rational(1)
  val ten = Rational(10)
  val half: Rational = Rational("1/2")
  val NaN = new Rational(0,0)

  def apply(x: Int): Rational = apply(x.toLong)

  def apply(x: Long): Rational = new Rational(x, 1)

  def apply(x: BigDecimal): Rational = if (x.scale >= 0) {
    val e = BigDecimal.apply(10).pow(x.scale)
    normalize((x * e).toLongExact, e.longValue)
  }
  else
    Rational(x.toLongExact)

  def apply(x: String): Rational = {
    val rRat = """^\s*(-?\d+)\s*(\/\s*(-?\d+)\s*)?$""".r
    val rDec = """^-?(\d|(\d+,?\d+))*(\.\d+)?(e\d+)?$""".r
    x match {
      // XXX I don't understand why we need this first line -- but it IS necessary
      case rRat(n, _, null) => Rational(n.toLong)
      case rRat(n, _, d) => normalize(n.toLong, d.toLong)
      case rRat(n) => Rational(n.toLong)
      case rDec(w, _, f, null) => Rational(BigDecimal.apply(w + f))
      // FIXME implement properly the case where the fourth component is "eN"
      case rDec(w, _, f, e) => val b = BigDecimal.apply(w + f + e); Rational(b)
      case _ => throw new RationalException(s"invalid rational expression: $x")
    }
  }

  def normalize(n: Long, d: Long): Rational = {
    val g = gcd(math.abs(n), math.abs(d))
    g match {
      case 0 =>
        Rational.NaN
      case _ =>
        val f = d.signum
        apply(f * n / g, f * d / g)
    }

  }

  @tailrec private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  private def longAbs(a: Long): Long = if (a < 0) -a else a

  /**
    * Implicit conversion method from Int=>Rational.
    * This is used for example when comparing a Rational value with an Int
    * @param x an Int
    * @return a Rational with the same value as x
    */
  implicit def intToRational(x: Int): Rational = Rational(x)

  /**
    * Trait which enables Rational to be used in type classes where the context type is a Fractional (or Numeric or Ordering).
    */
  trait RationalIsFractional extends Fractional[Rational] {
    //Members declared in scala.math.Numeric
    def plus(x: Rational, y: Rational): Rational = x + y

    def minus(x: Rational, y: Rational): Rational = x - y

    def times(x: Rational, y: Rational): Rational = x * y

    def quot(x: Rational, y: Rational): Rational = x / y

    def negate(x: Rational): Rational = Rational(-x.n, x.d)

    def fromInt(x: Int): Rational = Rational(x)

    def rem(x: Rational, y: Rational): Rational = zero

    def toInt(x: Rational): Int = x.toInt

    def toLong(x: Rational): Long = x.toLong

    def toFloat(x: Rational): Float = x.toFloat

    def toDouble(x: Rational): Double = x.toDouble

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
