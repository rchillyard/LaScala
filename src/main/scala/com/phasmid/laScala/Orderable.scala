package com.phasmid.laScala

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.FP.optionToTry
import com.phasmid.laScala.values.Rational

import scala.util.{Success, Try}

/**
  * Type class Orderable.
  *
  * This combination of trait Orderable and implicit objects comprises the "type class" Orderable.
  *
  * This has been split off from Valuable by scalaprof on 7/10/16.
  *
  * CONSIDER extending Ordered instead of Ordering
  *
  * TODO move this into values package
  */
trait Orderable[X] extends Ordering[X] {
  /**
    * Method to introduce an X value from an X.
    * Yes, I know this seems nonsensical but it is necessary.
    *
    * @param x the value to be introduced
    * @return x as an X
    *         CONSIDER make unit return Try[X]
    */
  def unit(x: X): X

  /**
    * Method to introduce an X value from a String.
    *
    * @param s       the String from which we wish to parse an X
    * @param pattern (implicit) the pattern (template) of the String, for example, when parsing a date string, do we put year, month or day first?
    * @return a Try[X]
    */
  def fromString(s: String)(implicit pattern: String): Try[X]

  /**
    * Method to introduce an X value from a String key, given a lookup function.
    *
    * @param k the key to be looked up
    * @param f the lookup function. Typically, this will be the get function of a Map[String,X]
    * @return a Try[X]
    */
  def viaLookup(k: String, f: String => Option[X]): Try[X]

  /**
    * The identity for addition.
    *
    * @return zero as an X
    */
  def zero: X
}

object Orderable {

  trait OrderableInt extends Orderable[Int] {
    def unit(x: Int): Int = x

    def viaLookup(k: String, f: (String) => Option[Int]): Try[Int] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[Int] = Try(s.toInt)

    def zero: Int = 0

    def compare(x: Int, y: Int): Int = x.compare(y)
  }

  implicit object OrderableInt extends OrderableInt

  trait OrderableDouble extends Orderable[Double] {
    def unit(x: Double) = x

    def fromString(s: String)(implicit pattern: String = "") = Try(s.toDouble)

    def viaLookup(k: String, f: String => Option[Double]) = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def zero = 0.0

    def compare(x: Double, y: Double): Int = x.compare(y)
  }

  implicit object OrderableDouble extends OrderableDouble

  trait OrderableRational extends Orderable[Rational] {
    def unit(x: Rational) = x

    def fromString(s: String)(implicit pattern: String = "") = Try(Rational(s))

    def viaLookup(k: String, f: String => Option[Rational]) = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def zero = Rational.zero

    def compare(x: Rational, y: Rational): Int = x.compare(y)
  }

  implicit object OrderableRational extends OrderableRational

  trait OrderableString extends Orderable[String] {
    def unit(x: String): String = x

    def viaLookup(k: String, f: (String) => Option[String]): Try[String] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[String] = Success(s)

    def zero: String = ""

    def compare(x: String, y: String): Int = x.compareTo(y)
  }

  implicit object OrderableString extends OrderableString

  trait OrderableLocalDate extends Orderable[LocalDate] {
    def unit(x: LocalDate): LocalDate = x

    def viaLookup(k: String, f: (String) => Option[LocalDate]): Try[LocalDate] = optionToTry(f(k), new OrderableException(s"$k is not defined"))

    def fromString(s: String)(implicit pattern: String): Try[LocalDate] = Try(LocalDate.parse(s, if (pattern.isEmpty) isoFormatter else formatter(pattern)))

    def zero: LocalDate = LocalDate.now

    def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)

    val isoFormatter = DateTimeFormatter.ISO_LOCAL_DATE

    def formatter(s: String) = DateTimeFormatter.ofPattern(s)
  }

  implicit object OrderableLocalDate extends OrderableLocalDate

  class OrderableException(s: String) extends Exception(s)

}

