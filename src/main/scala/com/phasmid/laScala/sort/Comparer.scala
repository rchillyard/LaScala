/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.sort

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

/**
  * This trait defines the concept of a Comparer: something that can compare two T values and yield a Comparison.
  *
  * It is a type class
  *
  * CONSIDER: renaming this trait as Orderable. The only problem is that we already have an Orderable in LaScala.
  * Probably we should merge the two concepts.
  *
  * @tparam T the type of the objects to be compared
  */
trait Comparer[T] extends (((T, T)) => Comparison) with Serializable {
  self =>

  //noinspection ConvertExpressionToSAM
  def toOrdering: Ordering[T] = new Ordering[T]() {
    def compare(x: T, y: T): Int = self(x, y).toInt
  }

  def >(tt: (T, T)): Boolean = apply(tt.swap)().getOrElse(false)

  def <(tt: (T, T)): Boolean = apply(tt)().getOrElse(false)

  def ==(tt: (T, T)): Boolean = apply(tt)().isEmpty

  def >=(tt: (T, T)): Boolean = ! <(tt)

  def <=(tt: (T, T)): Boolean = ! >(tt)

  def !=(tt: (T, T)): Boolean = ! ==(tt)

  def compose(f: Comparison => Comparison): Comparer[T] = new Comparer[T]() {
    def apply(tt: (T, T)): Comparison = f(self(tt))
  }

  def orElse(o: Comparer[T]): Comparer[T] = new Comparer[T]() {
    def apply(tt: (T, T)): Comparison = self(tt).orElse(o(tt))
  }

  def invert: Comparer[T] = compose(_ flip)
}

object Comparer {

  implicit val intComparer: Comparer[Int] = Ordering[Int]
  implicit val strComparer: Comparer[String] = Ordering[String]

  implicit def convert[T](x: Ordering[T]): Comparer[T] = new Comparer[T] {
    def apply(tt: (T, T)) = Comparison(x.compare(tt._1, tt._2))
  }
}

trait Comparison extends (() => Option[Boolean]) {

  override def toString(): String = apply.toString

  def toInt: Int = apply match {
    case Some(b) => if (b) -1 else 1;
    case _ => 0
  }

  def orElse(c: => Comparison): Comparison = Comparison(apply.orElse(c()))

  def flip: Comparison = Comparison(for (v <- apply) yield !v)
}

case class Different(less: Boolean) extends Comparison {
  def apply: Option[Boolean] = Some(less)
}

case object Same extends Comparison {
  def apply: Option[Boolean] = None
}

object Comparison {
  val more = Different(false)
  val less = Different(true)

  def apply(x: Option[Boolean]): Comparison = x match {
    case Some(b) => Different(b);
    case _ => Same
  }

  def apply(x: Int): Comparison = x match {
    case 0 => Same;
    case _ => Comparison(Some(x < 0))
  }
}