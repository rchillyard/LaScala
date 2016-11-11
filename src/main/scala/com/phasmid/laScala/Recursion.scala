package com.phasmid.laScala

import com.phasmid.laScala.values.Orderable

import scala.annotation.tailrec

/**
  * Type class Counter.
  * An extension of Orderable with the ability to increment the value.
  *
  * Note: this is similar to Incrementable
  *
  * This combination of trait Counter and implicit objects comprises the "type class" Counter.
  */
trait Counter[X] extends Orderable[X] {
  /**
    * Method to increment an X value by an integral number of some unit.
    *
    * @param x the X value
    * @return result as an X
    */
  def increment(x: X): X
}

object Counter {

  trait IntCounter extends Orderable.OrderableInt with Counter[Int] {
    def increment(x: Int): Int = x + 1
  }
  implicit object intCounter extends IntCounter

  trait LongCounter extends Orderable.OrderableLong with Counter[Long] {
    def increment(x: Long): Long = x + 1
  }

  implicit object longCounter extends LongCounter
}

/**
  * Created by scalaprof on 11/9/16.
  */
object Recursion {

  /**
    * Generic tail-recursive method (implementation actually invokes the tail-recursive countRecurse).
    * Typically, this is used to traverse through a tree, starting at the root of the tree.
    *
    * @param f  the map function which takes a P and T and produces an S.
    * @param g  the reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
    * @param h  the function which builds the T list from the existing T list and the given T.
    * @param q  the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
    * @param ts a list of Ts to be worked on.
    * @param r  the current value of the result, i.e. the "accumulator".
    * @tparam T the underlying type of the work list.
    * @tparam S the return type of f.
    * @tparam R the result type.
    * @return   a value of R.
    */
  final def recurse[T, S, R](f: T => S, g: (R, S) => R, h: (List[T], T) => List[T], q: R => Boolean = { x: R => false })(ts: List[T], r: R) =
    countRecurse[T, S, R, Int]((p, t) => f(t), g, h, q)(ts, 0, r)

  /**
    * Generic tail-recursive method with counting.
    * Typically, this is used to traverse through a tree, starting at the root of the tree.
    *
    * @param f  the map function which takes a P and T and produces an S.
    * @param g  the reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
    * @param h  the function which builds the T list from the existing T list and the given T.
    * @param q  the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
    * @param ts a list of Ts to be worked on.
    * @param p  a counter which will be incremented on each recursive call to countRecurse
    * @param r  the current value of the result, i.e. the "accumulator".
    * @tparam T the underlying type of the work list.
    * @tparam S the return type of f.
    * @tparam R the result type.
    * @tparam P The type of the counter -- context-bound to Counter
    * @return   a value of R.
    */
  final def countRecurse[T, S, R, P: Counter](f: (P, T) => S, g: (R, S) => R, h: (List[T], T) => List[T], q: R => Boolean = { x: R => false })(ts: List[T], p: P, r: R) =
    genericCountRecurse(f, g, h, { pp: P => implicitly[Counter[P]].increment(pp) }, q)(ts, p, r)

  /**
    * Generic tail-recursive method with counting.
    * Typically, this is used to traverse through a tree, starting at the root of the tree.
    *
    * CONSIDER reformulating the quick-return function to take an S as its input, rather than an R. That would, however, not be trivial.
    *
    * @param f  the map function which takes a P and T and produces an S.
    * @param g  the reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
    * @param h  the function which builds the T list from the existing T list and the given T.
    * @param y  the function which yields the next P from a P
    * @param q  the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
    * @param ts a list of Ts to be worked on.
    * @param p  a counter which will be incremented on each recursive call to countRecurse
    * @param r  the current value of the result, i.e. the "accumulator".
    * @tparam T the underlying type of the work list.
    * @tparam S the return type of f.
    * @tparam R the result type.
    * @tparam P The type of the counter -- context-bound to Counter
    * @return   a value of R.
    */
  @tailrec
  final def genericCountRecurse[T, S, R, P: Counter](f: (P, T) => S, g: (R, S) => R, h: (List[T], T) => List[T], y: P => P, q: R => Boolean = { x: R => false })(ts: List[T], p: P, r: R): R =
    if (q(r))
      r
    else
      ts match {
        case Nil => r
        case t :: z => genericCountRecurse(f, g, h, y, q)(h(z, t), y(p), g(r, f(p, t)))
      }
}
