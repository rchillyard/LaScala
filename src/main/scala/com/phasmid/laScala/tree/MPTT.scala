package com.phasmid.laScala.tree

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T](index: Map[T,MPTTEntry[T]]) extends (T=>MPTTEntry[T]){

  /**
    * Method to determine if given node is within given subtree
    * @param subtree the subtree
    * @param node the node
    * @return true if subtree contains node
    */
  def contains(subtree: T, node: T): Option[Boolean] = for (e1 <- get(subtree); e2 <- get(node)) yield e1.contains(e2)

  override def toString = {
    val r = new mutable.StringBuilder()
    for ((k,v) <- index) r.append(s"$k -> $v\n")
    r.toString
  }

  override def apply(t: T): MPTTEntry[T] = index(t)
  def get(t: T): Option[MPTTEntry[T]] = index.get(t)
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
      case IndexedNode(n,l,r) => n.get match {
        case Some(z) => Some(MPTTEntry.apply(z)(l,r))
        case _ => None
      }
      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
    }
    def g(mptt: MPTT[T], e: Option[MPTTEntry[T]]): MPTT[T] = e match { case Some(me) => MPTT[T](mptt.index + (me.t->me)); case None => mptt }
    Parent.traverse[Node[T], Option[MPTTEntry[T]], MPTT[T]](f, g)(List(x), MPTT[T](Map[T, MPTTEntry[T]]()))
  }
}