/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import com.phasmid.laScala.predicate.PredicateException

import scala.language.implicitConversions
import scala.math.Numeric.{DoubleIsFractional, IntIsIntegral}
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * FuzzyParser is a parser-combinator which parses rules. [Wow, that's a surprise!]
  * Of course, it can also parse any of the constituents of a rule (expression, number, predicate, etc.).
  *
  * A RuleLike is defined as a sealed trait which is extended by several different case classes, each defining
  * a different method of combining rules together into a tree of rules.
  * In general, a leaf rule is a Condition and is made up of two features: a subject and a predicate.
  *
  * It's important to note that all aspects of these rules are String-based. There is absolutely no conversion to numeric
  * types anywhere in this class.
  *
  * The trait RuleLike has only one method: asRule which returns a Rule[String]
  *
  * Created by scalaprof on 5/30/16.
  */
class FuzzyParser extends JavaTokenParsers {

  def parseFuzzy[T: Fractional](s: String): Try[Fuzzy[T]] = {
    parseAll(fuzzy, s) match {
      // CHECK that the following cast is OK. I don't really think it is.
      case this.Success(p, _) => scala.util.Success(p.asInstanceOf[Fuzzy[T]])
      case this.Failure(x, _) => FuzzyParser.parseFailure(s, "expression", x)
      case this.Error(x, _) => FuzzyParser.parseFailure(s, "expression", x)
    }
  }

  def fuzzy: Parser[Fuzzy[_]] = ((fractionalNumber ~ opt(fuzziness)) | wholeNumber) ^^ {
    case x ~ fo => fo match {
      case Some(f: String) => Fuzzy[Double](x.toString, Fuzziness(f))(FuzzyParser.FractionalStringDouble, FuzzyParser.FractionalStringDouble)
      case _ => Exact(x.toString.toDouble)(FuzzyParser.FractionalStringDouble)
    }

    case w => Exact.fromInt[Int](w.toString.toInt)(FuzzyParser.FractionalInt)
  }

  def fuzziness: Parser[String] = """~""" | """(""" ~> wholeNumber <~ """)"""

  //  def fractionalNumber[T: FractionalString]: Parser[T] = (floatingPointNumber | rationalNumber | failure("problem with number")) ^^ {
  def fractionalNumber: Parser[String] = floatingPointNumber | failure("problem with number")

  //  def rationalNumber: Parser[Rational[Int]] = wholeNumber ~ "/" ~ wholeNumber ^^ { case n ~ x ~ d =>  Rational[Int](n+x+d) }

}

trait FromString[T] {
  def fromString(s: String): T
}

trait FractionalString[T] extends Fractional[T] with FromString[T]

object FuzzyParser {

  implicit object FractionalStringDouble extends DoubleIsFractional with FractionalString[Double] {
    def fromString(s: String): Double = s.toDouble

    def compare(x: Double, y: Double): Int = x.compare(y)
  }

  implicit object FractionalInt extends Fractional[Int] {
    def div(x: Int, y: Int): Int = throw FuzzyException(s"div is not supported for FractionalInt")

    def compare(x: Int, y: Int): Int = x.compare(y)

    def plus(x: Int, y: Int): Int = x + y

    def minus(x: Int, y: Int): Int = x - y

    def times(x: Int, y: Int): Int = x * y

    def negate(x: Int): Int = -x

    def fromInt(x: Int): Int = x

    def toInt(x: Int): Int = x

    def toLong(x: Int): Long = x.toLong

    def toFloat(x: Int): Float = x.toFloat

    def toDouble(x: Int): Double = x.toDouble
  }

  implicit object FromStringInt extends FromString[Int] with IntIsIntegral {
    def fromString(s: String): Int = s.toInt

    def compare(x: Int, y: Int): Int = x.compare(y)
  }

  // TODO eliminate?
  implicit object FractionalStringInt extends FromString[Int] with IntIsIntegral {
    def fromString(s: String): Int = s.toInt

    def compare(x: Int, y: Int): Int = x.compare(y)
  }

  def parseFailure[X](s: String, e: String, x: String): Try[X] = {
    scala.util.Failure(new PredicateException(s"""unable to parse "$s" as a $e: $x"""))
  }
}



