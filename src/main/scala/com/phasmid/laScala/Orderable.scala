package com.phasmid.laScala

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.FP.optionToTry

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
    * @param s the String from which we wish to parse an X
    * @param pattern (implicit) the pattern (template) of the String, for example, when parsing a date string, do we put year, month or day first?
    * @return a Try[X]
    */
  def fromString(s: String)(implicit pattern: String): Try[X]

  /**
    * Method to introduce an X value from a String key, given a lookup function.
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

  implicit object OrderableString extends Orderable[String] {
    override def unit(x: String): String = x

    override def viaLookup(s: String, f: (String) => Option[String]): Try[String] = optionToTry(f(s), new OrderableException(s"$s is not defined"))

    override def fromString(s: String)(implicit pattern: String): Try[String] = Success(s)

    override def zero: String = ""

    override def compare(x: String, y: String): Int = x.compareTo(y)
  }

  /**
    * TODO OrderableDate should be retired and we should use IncrementableLocalDate instead.
    */
  implicit object OrderableDate extends Orderable[LocalDate] {
    override def unit(x: LocalDate): LocalDate = x

    override def viaLookup(s: String, f: (String) => Option[LocalDate]): Try[LocalDate] = optionToTry(f(s), new OrderableException(s"$s is not defined"))

    override def fromString(s: String)(implicit pattern: String): Try[LocalDate] = Try(LocalDate.parse(s, if (pattern.isEmpty) isoFormatter else formatter(pattern)))

    override def zero: LocalDate = LocalDate.now

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)

    val isoFormatter = DateTimeFormatter.ISO_LOCAL_DATE

    def formatter(s: String) = DateTimeFormatter.ofPattern(s)
  }
  class OrderableException(s: String) extends Exception(s)

}

