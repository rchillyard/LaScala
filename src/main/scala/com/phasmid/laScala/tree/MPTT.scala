package com.phasmid.laScala.tree

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T](index: Map[T,MPTTEntry[T]]) {
  def contains(subtree: T, node: T): Option[Boolean] = for (e1 <- index.get(subtree); e2 <- index.get(node)) yield e1.contains(e2)
}

/**
  * CONSIDER why does this have to parameter sets?
  * @param t
  * @param pre
  * @param post
  * @tparam T
  */
case class MPTTEntry[T](t: T)(val pre: Long, val post: Long) {
  def contains(x: MPTTEntry[T]): Boolean = this.pre <= x.pre && this.post >= x.post
  override def toString = s"$t: $pre,$post"
}

object MPTT {
  def apply[T](x: IndexedNode[T]): MPTT[T] = {
    val hashMap = new mutable.HashMap[T,MPTTEntry[T]]()
    def f(n: Node[T]): Option[MPTTEntry[T]] = n match {
      case IndexedLeaf(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply(v)(l,r)
      case EmptyWithIndex => None
      case IndexedNode(n,l,r) => Some(MPTTEntry.apply(n.get.get)(l,r))
      case _ => throw new TreeException(s"cannot build MPTT from non-indexed node: $n")
    }
    def g(mptt: MPTT[T], e: Option[MPTTEntry[T]]): MPTT[T] = e match { case Some(x) => MPTT[T](mptt.index + (x.t->x)); case None => mptt }
    val mptt = Parent.traverse[Node[T],Option[MPTTEntry[T]],MPTT[T]](f,g)(List(x), MPTT[T](Map[T,MPTTEntry[T]]()))
    mptt
  }
}