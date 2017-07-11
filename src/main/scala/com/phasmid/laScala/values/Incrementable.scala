package com.phasmid.laScala.values

import java.time.LocalDate

import com.phasmid.laScala.fp.FP

import scala.util.{Success, Try}

/**
  * Type class Incrementable.
  * An extension of Orderable with the ability to increment the value by integral units and to initialize such a sequence.
  *
  * This combination of trait Incrementable and implicit objects comprises the "type class" Incrementable.
  *
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

  trait IncrementableString extends Orderable.OrderableString with Incrementable[String] {

    private val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    override def zero: String = letters.head.toString

    def increment(x: String, y: Int = 1, by: String = ""): Try[String] = by match {
      case "" =>
        val p = x.init
        val i = letters.indexOf(x.last)
        assert(i >= 0)
        val j = i + y
        val a = j / letters.length
        val b = j % letters.length
        val prefix = if (a == 0) Success(p) else if (p.isEmpty) increment(zero, a - 1) else increment(p, a)
        val suffix = Success(letters(b))
        FP.map2(prefix, suffix)(_ + _)
      case _ => throw new IncrementableException(s"unit: $by is not supported")
    }
  }

  implicit object IncrementableString extends IncrementableString

  class IncrementableException(s: String) extends Exception(s)

}

