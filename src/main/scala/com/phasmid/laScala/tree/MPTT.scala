package com.phasmid.laScala.tree

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T](index: Map[T,MPTTEntry[T]]) {
  def contains(subtree: T, node: T): Option[Boolean] = for (e1 <- index.get(subtree); e2 <- index.get(node)) yield e1.contains(e2)
}

/**
  * CONSIDER why does this have two parameter sets?
  * @param t the value
  * @param pre the pre-index
  * @param post the post-index
  * @tparam T the type of the value
  */
case class MPTTEntry[T](t: T)(val pre: Long, val post: Long) {
  def contains(x: MPTTEntry[T]): Boolean = this.pre <= x.pre && this.post >= x.post
  override def toString = s"$t: $pre,$post"
}

object MPTT {
  def apply[T](x: IndexedNode[T]): MPTT[T] = {
    val hashMap = new mutable.HashMap[T,MPTTEntry[T]]()
    def f(node: Node[T]): Option[MPTTEntry[T]] = node match {
      case IndexedLeaf(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply(v)(l,r)
      case EmptyWithIndex => None
      case IndexedNode(n,l,r) => Some(MPTTEntry.apply(n.get.get)(l,r))
      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
    }
    def g(mptt: MPTT[T], e: Option[MPTTEntry[T]]): MPTT[T] = e match { case Some(me) => MPTT[T](mptt.index + (me.t->me)); case None => mptt }
    Parent.traverse[Node[T], Option[MPTTEntry[T]], MPTT[T]](f, g)(List(x), MPTT[T](Map[T, MPTTEntry[T]]()))
  }
}