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
    * @tparam P a type which extends Parent, and thus has children of type T -- this "context bound" is implemented via a compiler-generated implicit parameter of type Parent[T].
    * @tparam S the return type of f, typically an Option[X] where X is something that can be combined with an R.
    * @tparam R the result type.
    * @return a value of R.
    */
  final def traverse[P: Parent, S, R](f: P => S, g: (R, S) => R, q: R => Boolean = { _: R => false })(ts: Seq[P], r: R): R = {
    // NOTE that p itself does not and MUST not form part of the result of function h
    val h = { (ps: Seq[P], p: P) => implicitly[Parent[P]].children(p).toList ++ ps }
    Recursion.recurse(f, g, h, q)(ts, r)
  }
}

