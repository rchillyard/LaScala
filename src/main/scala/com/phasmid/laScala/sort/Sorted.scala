/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.sort

import com.phasmid.laScala.fp.FP

import scala.annotation.tailrec
import scala.collection.parallel
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait Comparer[T] extends (((T, T)) => Comparison) {
  self =>

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

case class Sorted[T](ts: Seq[T])(implicit f: Comparer[T]) extends (() => Seq[T]) {

  implicit val ordering: Ordering[T] = f.toOrdering

  def sort(o: Comparer[T]): Sorted[T] = Sorted(ts)(f orElse o)

  def apply: Seq[T] = ts.sorted

  def async(implicit ec: ExecutionContext): Future[Seq[T]] = Future(apply)

  def parSort(implicit ec: ExecutionContext): Future[Seq[T]] = Sorted.mergeSort(ts)
}

object Sorted {
  def create[T: Ordering](ts: Seq[T]): Sorted[T] = Sorted(ts)(implicitly[Ordering[T]])

  def parSort[T: Ordering](tst: (Seq[T], Seq[T]))(implicit ec: ExecutionContext): Future[Seq[T]] = map2(Future(tst._1.sorted), Future(tst._2.sorted))(merge)

  def mergeSort[T: Ordering](ts: Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = parSort(ts splitAt (ts.length/2))

  def merge[T: Ordering](ts1: Seq[T], ts2: Seq[T]): Seq[T] = {
    val ordering = implicitly[Ordering[T]]
    @tailrec def inner(r: Seq[T], xs: Seq[T], ys: Seq[T]): Seq[T] = (xs, ys) match {
      case (_, Nil) => r ++ xs
      case (Nil, _) => r ++ ys
      case (x :: xs1, y :: ys1) =>
        if (ordering.lt(x, y)) inner(r :+ x, xs1, ys)
        else inner(r :+ y, xs, ys1)
    }
    inner(Nil, ts1, ts2)
  }

  def map2[T: Ordering](t1f: Future[Seq[T]], t2f: Future[Seq[T]])(f: (Seq[T], Seq[T]) => Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = for {t1 <- t1f; t2 <- t2f} yield f(t1, t2)

}