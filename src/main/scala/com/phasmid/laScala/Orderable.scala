package com.phasmid.laScala

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.FP.optionToTry

import scala.util.Try

/**
  * Type class Orderable.
  *
  * This combination of trait Orderable and implicit objects comprises the "type class" Orderable.
  *
  * This has been split off from Valuable by scalaprof on 7/10/16.
  */
trait Orderable[X] extends Ordering[X] {
  // CONSIDER make unit return Try[X]
  def unit(x: X): X

  def fromString(s: String)(implicit pattern: String): Try[X]

  def viaLookup(s: String, f: String => Option[X]): Try[X]

  def zero: X
}

object Orderable {

  implicit object OrderableDate extends Orderable[LocalDate] {
    override def unit(x: LocalDate): LocalDate = x

    override def viaLookup(s: String, f: (String) => Option[LocalDate]): Try[LocalDate] = optionToTry(f(s), new OrderableException(s"$s is not defined"))

    override def fromString(s: String)(implicit pattern: String): Try[LocalDate] = Try(LocalDate.parse(s, if (pattern.isEmpty) isoFormatter else formatter(pattern)))

    override def zero: LocalDate = LocalDate.now

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)

    val isoFormatter = DateTimeFormatter.ISO_LOCAL_DATE

    def formatter(s: String) = DateTimeFormatter.ofPattern(s)
  }

  //  implicit object OrderableProduct extends Orderable[Product] {
  //    def unit(x: Product) = x
  //
  //    val sTuple = """\((\d+),(\d+)\)|(\d+),(\d+)""".r
  //    def fromString(s: String): Try[Product] = s match {
  //      case sTuple(x,y,_,_) => Try(x.toInt,y.toInt)
  //      case sTuple(_,_,x,y) => Try(x.toInt,y.toInt)
  //    }
  //
  //    def viaLookup(s: String, f: String => Option[Product]) = optionToTry(f(s), new OrderableException(s"$s is not defined"))
  //
  //    def zero = (0,0)
  //
  //    def different(a1: Any, a2: Any): Int = a1 match {
  //      case a: Ordering =>
  //        a2 match {
  //          case b: Ordering =>
  //        }
  //      case _ => 0
  //    }
  //
  //    /**
  //      * Compare tuples x and y where x is most significant
  //      *
  //      * @param x the left-hand comparand
  //      * @param y the right-hand comparand
  //      * @return 0, -1 or +1 as appropriate
  //      */
  //    def compare(x: Product, y: Product): Int = x.productIterator zip y.productIterator find (different(_,_))
  //  }

  implicit object OrderableIntInt extends Orderable[(Int, Int)] {
    def unit(x: (Int, Int)) = x

    val sTuple = """\((\d+),(\d+)\)|(\d+),(\d+)""".r

    def fromString(s: String)(implicit pattern: String = "") = s match {
      case sTuple(x, y, _, _) => Try(x.toInt, y.toInt)
      case sTuple(_, _, x, y) => Try(x.toInt, y.toInt)
    }

    def viaLookup(s: String, f: String => Option[(Int, Int)]) = optionToTry(f(s), new OrderableException(s"$s is not defined"))

    def zero = (0, 0)

    /**
      * Compare tuples x and y where x is most significant
      *
      * @param x the most significant comparand
      * @param y the least significant comparand
      * @return 0, -1 or +1 as appropriate
      */
    def compare(x: (Int, Int), y: (Int, Int)): Int = {
      val cf = x._1.compare(y._1)
      if (cf == 0) x._2.compare(y._2)
      else cf
    }
  }

  implicit object OrderableIntIntInt extends Orderable[(Int, Int, Int)] {
    def unit(x: (Int, Int, Int)) = x

    val sTuple = """\((\d+),(\d+),(\d+)\)|(\d+),(\d+),(\d+)""".r

    def fromString(s: String)(implicit pattern: String = "") = s match {
      case sTuple(x, y, z, _, _, _) => Try(x.toInt, y.toInt, z.toInt)
      case sTuple(_, _, _, x, y, z) => Try(x.toInt, y.toInt, z.toInt)
    }

    def zero = (0, 0, 0)

    def viaLookup(s: String, f: (String) => Option[(Int, Int, Int)]): Try[(Int, Int, Int)] = optionToTry(f(s), new OrderableException(s"$s is not defined"))

    /**
      * Compare tuples x and y where x is most significant
      *
      * @param x the most significant comparand
      * @param y the least significant comparand
      * @return 0, -1 or +1 as appropriate
      */
    def compare(x: (Int, Int, Int), y: (Int, Int, Int)): Int = {
      val cf = x._1.compare(y._1)
      if (cf == 0) {
        val cf = x._2.compare(y._2)
        if (cf == 0) x._3.compare(y._3)
        else cf
      }
      else cf
    }
  }

  class OrderableException(s: String) extends Exception(s)

}

