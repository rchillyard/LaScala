package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.HasKey

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[K, T : HasKey](index: Map[K,MPTTEntry[K,T]]) extends (K=>MPTTEntry[K,T]){

  /**
    * Method to determine if given node is within given subtree
    * @param subtree the subtree
    * @param node the node
    * @return true if subtree contains node
    */
  def contains(subtree: K, node: K): Option[Boolean] = for (e1 <- get(subtree); e2 <- get(node)) yield e1.contains(e2)

  override def toString = {
    val r = new mutable.StringBuilder()
    for ((k,v) <- index) r.append(s"$k -> $v\n")
    r.toString
  }

  override def apply(t: K): MPTTEntry[K,T] = index(t)

  def get(t: K): Option[MPTTEntry[K,T]] = index.get(t)
}

/**
  * CONSIDER why does this have two parameter sets?
  * @param t the value
  * @param pre the pre-index
  * @param post the post-index
  * @tparam T the type of the value
  */
case class MPTTEntry[K, T : HasKey](k: K, t: T)(val pre: Long, val post: Long) {
  def contains(x: MPTTEntry[K,T]): Boolean = this.pre <= x.pre && this.post >= x.post
  // NOTE: we are explicitly casting the underlying type of key in T to K
  def key: K = implicitly[HasKey[T]].getKey(t).asInstanceOf[K]
  override def toString = s"$t: $pre,$post"
}

object MPTT {
//  def apply[K, T : HasKey](x: IndexedNode[T]): MPTT[K,T] = {
//    val hashMap = new mutable.HashMap[T,MPTTEntry[K,T]]()
//    def f(node: Node[T]): Option[MPTTEntry[K,T]] = node match {
//      case IndexedLeafWithKey(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply(v.key,v)(l,r)
//      case EmptyWithIndex => None
//      case IndexedNode(n,l,r) => n.get match {
//        case Some(z) => Some(MPTTEntry.apply(z)(l,r))
//        case _ => None
//      }
//      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
//    }
//    def g(mptt: MPTT[K,T], e: Option[MPTTEntry[K,T]]): MPTT[K,T] = e match { case Some(me) => MPTT[K,T](mptt.index + (me.key->me)); case None => mptt }
//    Parent.traverse[Node[T], Option[MPTTEntry[K,T]], MPTT[K,T]](f, g)(List(x), MPTT[K,T](Map[K, MPTTEntry[K,T]]()))
//  }
  def apply[K, V : HasKey](x: IndexedNode[Value[K,V]]): MPTT[K,V] = {
    val hashMap = new mutable.HashMap[K,MPTTEntry[K,V]]()
    def f(node: Node[Value[K,V]]): Option[MPTTEntry[K,V]] = node match {
      case IndexedLeafWithKey(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply[K,V](v.key,v.value)(l,r)
      case EmptyWithIndex => None
      case EmptyWithKeyAndIndex() => None
      case IndexedNode(n,l,r) => n.get match {
        case Some(v) => Some(MPTTEntry.apply[K,V](v.key,v.value)(l,r))
        case _ => None
      }
      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
    }
    def g(mptt: MPTT[K,V], e: Option[MPTTEntry[K,V]]): MPTT[K,V] = e match { case Some(me) => MPTT[K,V](mptt.index + (me.key->me)); case None => mptt }
    Parent.traverse[Node[Value[K,V]], Option[MPTTEntry[K,V]], MPTT[K,V]](f, g)(List(x), MPTT[K,V](Map[K, MPTTEntry[K,V]]()))
  }
}