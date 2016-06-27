package com.phasmid.laScala.parser

import com.phasmid.laScala.FP.optionToTry

import scala.util.{Failure, Success, Try}

/**
  * Type class Valuable.
  * This combination of trait Valuable and implicit objects comprises the "type class" Valuable.
  * It is based on Numeric but has more method definitions and is therefore much more useful for evaluating expressions.
  *
  * Created by scalaprof on 6/5/16.
  */
trait Valuable[X] extends Ordering[X] {
  def unit(x: X): X
  def plus(x: X, y: X): Try[X]
  def minus(x: X, y: X): Try[X]
  def negate(x: X): Try[X]
  def times(x: X, y: X): Try[X]
  def div(x: X, y: X): Try[X]
  def invert(x: X): Try[X]
  def pow(x: X, y: X): Try[X]
  def fromInt(x: Int): Try[X]
  def fromString(s: String): Try[X]
  def viaLookup(s: String, f: String=>Option[X]): Try[X]
  def zero:X
  def one: X
  def function0(x: Double): Try[X]
  def function1(f: (Double) => Double)(x: X): Try[X]
  def function2(f: (Double,Double) => Double)(x: X, y: X): Try[X]
}
object Valuable {
  implicit object ValuableDouble extends Valuable[Double] {
    def unit(x: Double) = x
    def plus(x: Double, y: Double) = Try(x+y)
    def minus(x: Double, y: Double) = Try(x-y)
    def negate(x: Double) = minus(zero,x)
    def times(x: Double, y: Double) = Try(x*y)
    def div(x: Double, y: Double) = Try(x/y)
    def invert(x: Double) = div(one,x)
    def pow(x: Double, y: Double): Try[Double] = Try(math.pow(x,y))
    def fromInt(x: Int) = Try(x.toDouble)
    def fromString(s: String) = Try(s.toDouble)
    def viaLookup(s: String, f: String=>Option[Double]) = optionToTry(f(s),new ValuableException(s"$s is not defined"))
    def zero = 0.0
    def one = 1.0
    def compare(x: Double, y: Double): Int = x.compare(y)
    def function0(x: Double): Try[Double] = Try(x)
    def function1(f: (Double) => Double)(x: Double): Try[Double] = Try(f(x))
    def function2(f: (Double,Double) => Double)(x: Double, y: Double): Try[Double] = Try(f(x,y))
  }
  implicit object ValuableInt extends Valuable[Int] {
    def unit(x: Int) = x
    def plus(x: Int, y: Int) = Try(x+y)
    def minus(x: Int, y: Int) = Try(x-y)
    def negate(x: Int) = minus(zero,x)
    def times(x: Int, y: Int) = Try(x*y)
    def div(x: Int, y: Int) = Try(if (x%y==0) x/y else throw new ValuableException("integer division leaves remainder"))
    def invert(x: Int) = Failure(new ValuableException("cannot invert an Int"))
    def pow(x: Int, y: Int): Try[Int] = Try(Seq.fill[Int](y)(x).product)
    def fromInt(x: Int) = Success(x)
    def fromString(s: String) = Try(s.toInt)
    def viaLookup(s: String, f: String=>Option[Int]) = optionToTry(f(s),new ValuableException(s"$s is not defined"))
    def zero = 0
    def one = 1
    def compare(x: Int, y: Int): Int = x.compare(y)
    def function0(x: Double): Try[Int] = Failure(new ValuableException("cannot apply an arbitrary function0"))
    def function1(f: (Double) => Double)(x: Int): Try[Int] = Failure(new ValuableException("cannot apply an arbitrary function1 to an Int"))
    def function2(f: (Double,Double) => Double)(x: Int, y: Int): Try[Int] = Failure(new ValuableException("cannot apply an arbitrary function2 to an Int"))
  }
  class ValuableException(s: String) extends Exception(s)
}

