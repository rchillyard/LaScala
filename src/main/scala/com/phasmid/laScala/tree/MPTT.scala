package com.phasmid.laScala.tree

//import com.phasmid.laScala.tree.Parent

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T](index: Map[T,MPTTEntry[T]]) {
  def contains(subtree: T, node: T): Option[Boolean] = for (e1 <- index.get(subtree); e2 <- index.get(node)) yield e1.contains(e2)
}

case class MPTTEntry[T](t: T)(val pre: Long, val post: Long) {
  def contains(x: MPTTEntry[T]): Boolean = this.pre <= x.pre && this.post >= x.post
}

object MPTT {



  def apply[T](x: IndexedNode[T]): MPTT[T] = {
    val hashMap = new mutable.HashMap[T,MPTTEntry[T]]()
    def f(n: Node[T]): MPTTEntry[T] = n match {
      case IndexedNode(n,i) => MPTTEntry.apply(n.get.get)(i, 0)
      case _ => throw new TreeException("cannot build MPTT from non-indexed node")
    }
    def g(mptt: MPTT[T], e: MPTTEntry[T]): MPTT[T] = MPTT(mptt.index + (e.t->e))
    val mptt = Parent.traverse[Node[T],MPTTEntry[T],MPTT[T]](f,g)(List(x), MPTT(Map[T,MPTTEntry[T]]()))
    mptt
  }
}