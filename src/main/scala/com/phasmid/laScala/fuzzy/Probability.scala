package com.phasmid.laScala.fuzzy

import com.phasmid.laScala.values.Rational

import scala.math.Numeric.DoubleIsFractional


trait Probability[-T, +X] extends (T=>X){
  def isPdf: Boolean

  def &[U<:T, Y>:X: Numeric](p: Probability[U,Y]): Probability[U,Y]
  def |[U<:T, Y>:X: Numeric](p: Probability[U,Y]): Probability[U,Y]
  def ! : Probability[T,X]
  def *[Y>:X: Numeric](y: Y) : Probability[T, Y]
}

abstract class ProbabilityBase[-T, +X: Numeric] extends Probability[T,X] { self =>

  def build[U<:T, Y>:X: Numeric](f: U=>Y): Probability[U, Y]

  def &[U<:T, Y>:X: Numeric](p: Probability[U,Y]): Probability[U,Y] = build(t => Probability.&(self(t),p(t)))
  def |[U<:T, Y>:X: Numeric](p: Probability[U,Y]): Probability[U,Y] = build(t => Probability.|(self(t),p(t)))
  def ! : Probability[T, X] = build(t => Probability.!(self(t)))
  def *[Y>:X: Numeric](y: Y) : Probability[T, Y] = build(t => Probability.*(self(t),y))
}

case class PDF[T, X: Numeric](f: T=>X) extends ProbabilityBase[T,X] { self =>
  def isPdf: Boolean = true

  def apply(t: T): X = f(t)

  def build[U <: T, Y >: X: Numeric](f: U => Y): Probability[U, Y] = PDF(f)
}

object Probability {
  def step[T: Ordering,X: Numeric](s: T): Probability[T,X] = {
    val xn = implicitly[Numeric[X]]
    PDF(t => if (implicitly[Ordering[T]].compare(t,s)<0) xn.zero else xn.one )
  }
  def bounded[T,X](s1: T, s2: T)(implicit tf: Fractional[T], xf: CompleteNumeric[X]): Probability[T,X] = {
    (step(s1)(tf,xf) & (step(s2)(tf,xf) !)) * xf.convert(tf.div(tf.one, tf.minus(s2, s1)))
  }
  def ![X: Numeric](x: X): X = implicitly[Numeric[X]].negate(x)
  def &[X: Numeric](x1: X, x2: X): X = implicitly[Numeric[X]].times(x1,x2)
  def |[X: Numeric](x1: X, x2: X): X = {
    val xn = implicitly[Numeric[X]]
    xn.minus(xn.plus(x1, x2), & (x1, x2))
  }
  def *[X: Numeric](x1: X, x2: X): X = {
    val xn = implicitly[Numeric[X]]
    xn.abs(xn.times(x1, x2))
  }
}

abstract class ProbabilityEventBase[T, X: Numeric] extends ProbabilityBase[T,X] {
  def isPdf: Boolean = false

  def p(t: T): X

  def apply(t: T): X = p(t)

  /**
    * For now, at least, when we combine probability events, we create a new PDF rather than retaining either shape of the inputs.
    * @param f
    * @tparam U
    * @tparam Y
    * @return
    */
  def build[U <: T, Y >: X: Numeric](f: U => Y): Probability[U, Y] = PDF(f)
}

case class EqualProbabilityEvent[T, X: Fractional](n: Int) extends ProbabilityEventBase[T,X] {

  def p(t: T): X = {
    val xf = implicitly[Fractional[X]]
    xf.div(xf.one,xf.fromInt(n))
  }
}

case class IndependentEvents[T, X: Fractional](events: (T,X)*) extends ProbabilityEventBase[T,X] {
  lazy val total = events.unzip._2.sum

  def p(t: T): X = {
    val xf = implicitly[Fractional[X]]
    (for (x <- events.find(_._1==t) map (_._2)) yield xf.div(x, total)).getOrElse(xf.zero)
  }
}

trait CompleteNumeric[N] extends Fractional[N] {
  def convert[F: Fractional](f: F): N = {
    import com.phasmid.laScala.values.FiniteIntegral.IntIsFiniteIntegral
    val b = Rational(implicitly[Fractional[F]].toDouble(f))
    this.div(this.fromInt(b.n), this.fromInt(b.d))
  }
}

object CompleteNumeric {
  trait CompleteNumericDouble extends CompleteNumeric[Double] with DoubleIsFractional {
    def compare(x: Double, y: Double): Int = x.compareTo(y)
  }
  implicit object CompleteNumericDouble extends CompleteNumericDouble
}