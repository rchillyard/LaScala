package com.phasmid.laScala.tree

import com.phasmid.laScala.Recursion

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
    * @tparam T a type which extends Parent, and thus has children of type T -- this "context bound" is implemented via a compiler-generated implicit parameter of type Parent[T].
    * @tparam R the result type.
    * @return   a value of R.
    */
  final def traverse[T : Parent, S, R](f: T => S, g: (R, S) => R, q: R => Boolean = {x: R => false})(ts: List[T], r: R): R = {
    val z = {(ts: List[T], s: T) => ts++ implicitly[Parent[T]].children(s) }
    Recursion.recurse(f, g, z, q)(ts, r)
  }
}

