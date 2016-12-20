package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.{Spy}

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T](index: Map[String, MPTTEntry[T]]) extends (String => MPTTEntry[T]) {

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
case class MPTTEntry[T](k: String, t: T)(val pre: Long, val post: Long)(implicit ko: KeyOps[String,T]) {
  implicit val logger = Spy.getLogger(getClass)
  def contains(x: MPTTEntry[T]): Boolean = Spy.spy(s"contains: MPTTEntry($k,$t)($pre,$post); $x",this.pre <= x.pre && this.post >= x.post)
  def key: String = ko.getKeyFromValue(t).toString
  override def toString = s"$t: $pre,$post"
}

object MPTTEntry {
//  implicit object HasKeyStringString extends HasKey[String] {
//    type K = String
//    def getKey(v: String): String = v
//    def createValueFromKey(k: K): String = null.asInstanceOf[String] // FIXME
//  }
  def apply(k: String)(pre: Long, post: Long)(implicit ko: KeyOps[String,String]): MPTTEntry[String] = apply(k,k)(pre,post)
}

object MPTT {
//  implicit object StringKeyOps extends KeyOps[String,T] {
//    def getKey(v: T): String = v.toString //CHECK
//  }

  def apply[T](x: IndexedNode[T])(implicit ko: KeyOps[String,T]): MPTT[T] = {
    def f(node: Node[T]): Option[MPTTEntry[T]] = node match {
      case IndexedLeafWithKey(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply(ko.getKeyFromValue(v), v)(l, r)
      case EmptyWithIndex => None
      case IndexedNode(n,l,r) => n.get match {
        case Some(v) => Some(MPTTEntry.apply(ko.getKeyFromValue(v), v)(l, r))
        case _ => None
      }
      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
    }

    def g(mptt: MPTT[T], e: Option[MPTTEntry[T]]): MPTT[T] = e match {
      case Some(me) => MPTT[T](mptt.index + (me.key -> me));
      case None => mptt
    }

    Parent.traverse[Node[T], Option[MPTTEntry[T]], MPTT[T]](f, g)(List(x), MPTT[T](Map[String, MPTTEntry[T]]()))
  }

  // TODO eliminate this -- it is now identical to apply
def createValuedMPTT[V](x: IndexedNode[V])(implicit ko: KeyOps[String,V]): MPTT[V] = {
  def f(node: Node[V]): Option[MPTTEntry[V]] = node match {
    case IndexedLeafWithKey(lo, ro, v) => for (l <- lo; r <- ro) yield MPTTEntry.apply(ko.getKeyFromValue(v), v)(l, r)
      case EmptyWithIndex => None
      case IndexedNode(n,l,r) => n.get match {
        case Some(v) => Some(MPTTEntry.apply[V](ko.getKeyFromValue(v), v)(l, r))
        case _ => None
      }
      case _ => throw TreeException(s"cannot build MPTT from non-indexed node: $node")
    }

  def g(mptt: MPTT[V], e: Option[MPTTEntry[V]]): MPTT[V] = e match {
    case Some(me) => MPTT[V](mptt.index + (me.key -> me));
    case None => mptt
  }

  Parent.traverse[Node[V], Option[MPTTEntry[V]], MPTT[V]](f, g)(List(x), MPTT[V](Map[String, MPTTEntry[V]]()))
  }
}