package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.Spy

import scala.collection.mutable

/**
  * Created by scalaprof on 11/7/16.
  */
case class MPTT[T](index: Map[String, MPTTEntry[T]]) extends (String => MPTTEntry[T]) {

  /**
    * Method to determine if given node is within given subtree
    *
    * @param subtree the subtree
    * @param node    the node
    * @return true if subtree contains node
    */
  def contains(subtree: String, node: String): Option[Boolean] = for (e1 <- get(subtree); e2 <- get(node)) yield e1.contains(e2)

  override def toString: String = {
    val r = new mutable.StringBuilder()
    for ((k, v) <- index) r.append(s"$k -> $v\n")
    r.toString
  }

  override def apply(t: String): MPTTEntry[T] = index(t)

  def get(t: String): Option[MPTTEntry[T]] = index.get(t)
}

/**
  * CONSIDER why does this have two parameter sets?
  *
  * @param t    the value
  * @param pre  the pre-index
  * @param post the post-index
  * @tparam T the type of the value
  */
case class MPTTEntry[T](k: String, t: T)(val pre: Long, val post: Long)(implicit vo: ValueOps[String, T]) {
  private implicit val logger = Spy.getLogger(getClass)

  def contains(x: MPTTEntry[T]): Boolean = this.pre <= x.pre && this.post >= x.post

  def key: String = vo.getKeyFromValue(t).toString

  override def toString = s"$t: $pre,$post"
}

object MPTTEntry {
  def apply(k: String)(pre: Long, post: Long)(implicit vo: ValueOps[String, String]): MPTTEntry[String] = apply(k, k)(pre, post)
}

object MPTT {
  def apply[T](x: IndexedNode[T])(implicit vo: ValueOps[String, T]): MPTT[T] = {
    def f(node: Node[T]): Option[MPTTEntry[T]] = node match {
      case EmptyWithIndex => None
      case IndexedNode(n, l, r) => n.get match {
        // XXX: the extra annotation [T] and explicit reference to vo in the following is only necessary when compiling against 2.10
        case Some(v) => Some(MPTTEntry.apply[T](vo.getKeyFromValue(v), v)(l, r)(vo))
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
}