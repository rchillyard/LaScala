/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

import com.phasmid.laScala.fp.FP
import com.phasmid.laScala.values.Incrementable

import scala.util._

/**
  * This trait defines the concept of something which is part of a sequence, i.e. something which has a next instance.
  *
  * Created by scalaprof on 7/11/17.
  *
  * @tparam T the underlying type of this Sequential
  *           NOTE: once Dotty comes out, we may be able to require that T implement Ordering right here.
  */
trait Sequential[T] {

  /**
    * Method to try to create a next instance.
    *
    * @param isSnapshot (defaults to false) If true, then the next Sequential will be a snapshot.
    *                   The expected behavior is that when isSnapshot changes from false to true, it will be
    *                   accompanied by a change in sequence (where the new Sequential is ordered after this),
    *                   otherwise not.
    * @return Try[T]
    */
  def next(isSnapshot: Boolean = false): Try[T]
}

/**
  * Ordered version of the Sequential trait.
  * We explicitly implement Ordered[T] in addition to Ordering[T].
  * It would be quite possible to do comparisons using implicit conversions, but this does make things easier.
  *
  * @tparam T the underlying type of this Sequential
  */
trait OrderedSequential[T] extends Sequential[T] with Ordering[T] with Ordered[T]

/**
  * This is a simple implementation of the Sequential trait which does not allow for sub-versions.
  *
  * @param t          the value of this IncrementalSequential
  * @param isSnapshot (defaults to false) true if this is a snapshot
  * @param by         the unit of the increment
  * @tparam T the underlying type of this Sequential: it is defined that there exist an implicit value of Incrementable[T] available --
  *           in practice, this typically means an integer of some sort, as String or a LocalDate.
  */
case class IncrementableSequential[T: Incrementable](t: T, isSnapshot: Boolean = false, by: String = "") extends OrderedSequential[IncrementableSequential[T]] {
  /**
    *
    * @param isSnapshot (defaults to false) If true, then the next Sequential will be a snapshot.
    *                   The expected behavior is that when isSnapshot changes from false to true, it will be
    *                   accompanied by a change in sequence (where the new Sequential is ordered after this),
    *                   otherwise not.
    * @return Try[T]
    */
  def next(isSnapshot: Boolean = false): Try[IncrementableSequential[T]] =
    if (this.isSnapshot)
      if (isSnapshot) Success(this)
      else Success(IncrementableSequential(t, isSnapshot = false, by))
    else for (_t <- implicitly[Incrementable[T]].increment(t, 1, by)) yield IncrementableSequential(_t, isSnapshot, by)

  /**
    * Method to compare two IncrementalSequential instances
    *
    * @param i1 the first
    * @param i2 the second
    * @return the comparison of their underlying values, assuming it is non-zero,
    *         otherwise, we compare the isSnapshot statuses (true comes before false)
    */
  def compare(i1: IncrementableSequential[T], i2: IncrementableSequential[T]): Int = {
    def compareValues(x: IncrementableSequential[T], y: IncrementableSequential[T]) = implicitly[Incrementable[T]].compare(x.t, y.t)

    def compareSnapshots(x: IncrementableSequential[T], y: IncrementableSequential[T]) = -x.isSnapshot.compareTo(y.isSnapshot)

    FP.discriminate(i1, i2)(compareValues)(compareSnapshots)
  }

  /**
    * Method to compare this IncrementalSequential with an IncrementableSequential
    *
    * @param that the other IncrementableSequential
    * @return the comparison of this and that
    */
  def compare(that: IncrementableSequential[T]): Int = compare(this, that)
}