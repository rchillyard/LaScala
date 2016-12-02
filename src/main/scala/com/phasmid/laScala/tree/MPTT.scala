package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.HasKey

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T: HasKey](index: Map[String, MPTTEntry[T]]) extends (String => MPTTEntry[T]) {

  /**
    * Method to determine if given node is within given subtree
    * @param subtree the subtree
    * @param node the node
    * @return true if subtree contains node
    */
  def contains(subtree: String, node: String): Option[Boolean] = for (e1 <- get(subtree); e2 <- get(node)) yield e1.contains(e2)

  override def toString: String = {
    val r = new mutable.StringBuilder()
    for ((k,v) <- index) r.append(s"$k -> $v\n")
    r.toString
  }

  override def apply(t: String): MPTTEntry[T] = index(t)

  def get(t: String): Option[MPTTEntry[T]] = index.get(t)
}

/**
  * CONSIDER why does this have two parameter sets?
  * @param t the value
  * @param pre the pre-index
  * @param post the post-index
  * @tparam T the type of the value
  */
case class MPTTEntry[T: HasKey](k: String, t: T)(val pre: Long, val post: Long) {
  def contains(x: MPTTEntry[T]): Boolean = this.pre <= x.pre && this.post >= x.post

  // NOTE: we are explicitly casting the underlying type of key in T to String
  def key: String = implicitly[HasKey[T]].getKey(t).asInstanceOf[String]
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
def apply[V: HasKey](x: IndexedNode[Value[V]]): MPTT[V] = {
  //    val hashMap = new mutable.HashMap[String,MPTTEntry[V]]()
  def f(node: Node[Value[V]]): Option[MPTTEntry[V]] = node match {
    case IndexedLeafWithKey(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply[V](v.key, v.value)(l, r)
      case EmptyWithIndex => None
      case EmptyWithKeyAndIndex() => None
      case IndexedNode(n,l,r) => n.get match {
        case Some(v) => Some(MPTTEntry.apply[V](v.key, v.value)(l, r))
        case _ => None
      }
      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
    }

  def g(mptt: MPTT[V], e: Option[MPTTEntry[V]]): MPTT[V] = e match {
    case Some(me) => MPTT[V](mptt.index + (me.key -> me));
    case None => mptt
  }

  Parent.traverse[Node[Value[V]], Option[MPTTEntry[V]], MPTT[V]](f, g)(List(x), MPTT[V](Map[String, MPTTEntry[V]]()))
  }
}