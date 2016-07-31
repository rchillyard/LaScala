package com.phasmid.laScala

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.FP.optionToTry

import scala.util.Try

/**
  * Type class Incrementable.
  * An extension of Orderable with the ability to increment the value by integral units.
  *
  * This combination of trait Incrementable and implicit objects comprises the "type class" Incrementable.
  *
  * TODO move this into values package
  */
trait Incrementable[X] extends Orderable[X] {
  /**
    * Method to increment an X value by an integral number of some unit.
    *
    * @param x  the X value
    * @param y  the increment
    * @param by an indication of the size of one increment
    * @return result as an Try[X]
    */
  def increment(x: X, y: Int = 1, by: String = ""): Try[X]
}

object Incrementable {

  implicit object IncrementableLocalDate extends Incrementable[LocalDate] {
    override def unit(x: LocalDate): LocalDate = x

    override def viaLookup(s: String, f: (String) => Option[LocalDate]): Try[LocalDate] = optionToTry(f(s), new IncrementableException(s"$s is not defined"))

    override def fromString(s: String)(implicit pattern: String): Try[LocalDate] = Try(LocalDate.parse(s, if (pattern.isEmpty) isoFormatter else formatter(pattern)))

    override def zero: LocalDate = LocalDate.now

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)

    val isoFormatter = DateTimeFormatter.ISO_LOCAL_DATE

    def formatter(s: String) = DateTimeFormatter.ofPattern(s)

    def increment(x: LocalDate, y: Int = 1, by: String = ""): Try[LocalDate] = Try {
      val f = by match {
        case "d" | "" => x.plusDays _
        case "w" => x.plusWeeks _
        case "m" => x.plusMonths _
        case "y" => x.plusYears _
        case _ => throw new IncrementableException(s"unit: $by is not supported")
      }
      f(y)
    }
  }

  class IncrementableException(s: String) extends Exception(s)

}

