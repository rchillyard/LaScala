package com.phasmid.laScala.parser

import com.phasmid.laScala.FP.{map2,optionToTry}

import scala.util.{Try,Success,Failure}

/**
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
  def fromInt(x: Int): Try[X]
  def fromString(s: String): Try[X]
  def viaLookup(s: String, f: String=>Option[X]): Try[X]
  def zero:X
  def one: X
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
    def fromInt(x: Int) = Try(x.toDouble)
    def fromString(s: String) = Try(s.toDouble)
    def viaLookup(s: String, f: String=>Option[Double]) = optionToTry(f(s),new ValuableException(s"$s is not defined"))
    def zero = 0.0
    def one = 1.0
    def compare(x: Double, y: Double): Int = x.compare(y)
  }
  implicit object ValuableInt extends Valuable[Int] {
    def unit(x: Int) = x
    def plus(x: Int, y: Int) = Try(x+y)
    def minus(x: Int, y: Int) = Try(x-y)
    def negate(x: Int) = minus(zero,x)
    def times(x: Int, y: Int) = Try(x*y)
    def div(x: Int, y: Int) = Try(if (x%y==0) x/y else throw new ValuableException("integer division leaves remainder"))
    def invert(x: Int) = Failure(new ValuableException("cannot invert an Int"))
    def fromInt(x: Int) = Success(x)
    def fromString(s: String) = Try(s.toInt)
    def viaLookup(s: String, f: String=>Option[Int]) = optionToTry(f(s),new ValuableException(s"$s is not defined"))
    def zero = 0
    def one = 1
    def compare(x: Int, y: Int): Int = x.compare(y)
  }
  class ValuableException(s: String) extends Exception(s)
}

