package com.phasmid.laScala.tree2

import com.phasmid.laScala.values.Orderable

import scala.annotation.tailrec
import scala.language.implicitConversions

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
    * @param f   the (leaf) map function which takes a P and an A and produces an S.
    * @param g   the (leaf) reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
    * @param h   the (branch) function which builds the working TreeSeq[A] from the existing TreeSeq[A] and a TreeSeq[A] (i.e. the children)
    * @param q   the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
    * @param ats a TreeSeq[A], the list of trees yet to be worked on.
    * @param r   the current value of the result, i.e. the "accumulator".
    * @tparam A the underlying type of the Tree elements.
    * @tparam S the return type of f, typically an Option[X] where X is something that can be combined with an R.
    * @tparam R the result type.
    * @return a value of R.
    */
  final def recurse[A, S, R](f: A => S, g: (R, S) => R, h: (TreeSeq[A], TreeSeq[A]) => TreeSeq[A], q: R => Boolean = { x: R => false })(ats: TreeSeq[A], r: R) =
  countRecurse[A, S, R, Int]((p, t) => f(t), g, h, q)(ats, 0, r)

  /**
    * Generic tail-recursive method with counting.
    * Typically, this is used to traverse through a tree, starting at the root of the tree.
    *
    * @param f   the (leaf) map function which takes a P and an A and produces an S.
    * @param g   the (leaf) reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
    * @param h   the (branch) function which builds the working TreeSeq[A] from the existing TreeSeq[A] and a TreeSeq[A] (i.e. the children)
    * @param q   the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
    * @param ats a TreeSeq[A], the list of trees yet to be worked on.
    * @param p   a counter which will be incremented on each recursive call to countRecurse
    * @param r   the current value of the result, i.e. the "accumulator".
    * @tparam A the underlying type of the Tree elements.
    * @tparam S the return type of f, typically an Option[X] where X is something that can be combined with an R.
    * @tparam R the result type.
    * @tparam P The type of the counter -- context-bound to Counter
    * @return a value of R.
    */
  final def countRecurse[A, S, R, P: Counter](f: (P, A) => S, g: (R, S) => R, h: (TreeSeq[A], TreeSeq[A]) => TreeSeq[A], q: R => Boolean = { x: R => false })(ats: TreeSeq[A], p: P, r: R) =
  genericCountRecurse(f, g, h, { pp: P => implicitly[Counter[P]].increment(pp) }, q)(ats, p, r)

  /**
    * Generic tail-recursive method with counting.
    * Typically, this is used to traverse through a tree, starting at the root of the tree.
    *
    * CONSIDER reformulating the quick-return function to take an S as its input, rather than an R. That would, however, not be trivial.
    *
    * @param f   the (leaf) map function which takes a P and an A and produces an S.
    * @param g   the (leaf) reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
    * @param h   the (branch) function which builds the working TreeSeq[A] from the existing TreeSeq[A] and a TreeSeq[A] (i.e. the children)
    * @param y   the function which yields the next P from a P
    * @param q   the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
    * @param ats a TreeSeq[A], the list of trees yet to be worked on.
    * @param p   a counter which will be incremented on each recursive call to countRecurse
    * @param r   the current value of the result, i.e. the "accumulator".
    * @tparam A the underlying type of the Tree elements.
    * @tparam S the return type of f, typically an Option[X] where X is something that can be combined with an R.
    * @tparam R the result type.
    * @tparam P The type of the counter -- context-bound to Counter
    * @return a value of R.
    */
  @tailrec
  final def genericCountRecurse[A, S, R, P: Counter](f: (P, A) => S, g: (R, S) => R, h: (TreeSeq[A], TreeSeq[A]) => TreeSeq[A], y: P => P, q: R => Boolean = { x: R => false })(ats: TreeSeq[A], p: P, r: R): R =
  if (q(r))
    r
  else
    ats match {
      case Nil => r
      case Leaf(a) :: z => genericCountRecurse(f, g, h, y, q)(z, y(p), g(r, f(p, a)))
      case Branch(ts) :: z => genericCountRecurse(f, g, h, y, q)(h(z, ts), y(p), r)
    }
}

/**
  * A trait which expresses parenthood.
  * This defines a type class.
  *
  * @tparam T the underlying type of the Parent
  */
trait Parent[T] {
  /**
    * Get the children of a T.
    *
    * @param t the parent whose children we want
    * @return the children as a Seq of T
    */
  def children(t: T): Seq[T]
}

object Parent {

    /**
      * Generic tail-recursive tree-traversal method.
      *
      * @param f  the map function which takes a T and produces an S for just that parent object (not its children).
      * @param g  the reduce function which accumulates the result (the accumulator is passed as its first parameter and an S value is passed as the second).
      * @param q  the quick return function which, if q(r) yields true, the method immediately returns r. Defaults to always false.
      * @param ts a list of Ts to be worked on.
      * @param r  the current value of the result, i.e. the "accumulator".
      * @tparam A a type which extends Parent, and thus has children of type T -- this "context bound" is implemented via a compiler-generated implicit parameter of type Parent[T].
      * @tparam S the return type of f, typically an Option[X] where X is something that can be combined with an R.
      * @tparam R the result type.
      * @return   a value of R.
      */
    final def traverse[A : Parent, S, R](f: A => S, g: (R, S) => R, q: R => Boolean = {x: R => false})(ts: Seq[A], r: R): R = {
      val h = {(ts: TreeSeq[A], s: A) => implicitly[Parent[A]].children(s).toList ++ ts }
      Recursion.recurse(f, g, h, q)(ts, r)
      //f: A => S, g: (R, S) => R, h: (TreeSeq[A], TreeSeq[A]) => TreeSeq[A], q: R => Boolean = { x: R => false }
    }
}

