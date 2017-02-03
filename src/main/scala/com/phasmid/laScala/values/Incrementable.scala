package com.phasmid.laScala.values

import java.time.LocalDate

import scala.util.{Success, Try}

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
  def apply[X: Incrementable]: Incrementable[X] = implicitly[Incrementable[X]]

  trait IncrementableInt extends Orderable.OrderableInt with Incrementable[Int] {
    def increment(x: Int, y: Int = 1, by: String = ""): Try[Int] = by match {
      case "" => Success(x + y)
      case _ => throw new IncrementableException(s"unit: $by is not supported")
    }
  }

  implicit object IncrementableInt extends IncrementableInt

  trait IncrementableLong extends Orderable.OrderableLong with Incrementable[Long] {
    def increment(x: Long, y: Int = 1, by: String = ""): Try[Long] = by match {
      case "" => Success(x + y)
      case _ => throw new IncrementableException(s"unit: $by is not supported")
    }
  }

  implicit object IncrementableLong extends IncrementableLong

  trait IncrementableLocalDate extends Orderable.OrderableLocalDate with Incrementable[LocalDate] {
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

  implicit object IncrementableLocalDate extends IncrementableLocalDate

  class IncrementableException(s: String) extends Exception(s)

}

