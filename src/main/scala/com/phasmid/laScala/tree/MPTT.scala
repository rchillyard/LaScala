package com.phasmid.laScala.tree

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
  def apply[T](x: BinaryTree[T]): MPTT[T] = {
    val hashMap = new mutable.HashMap[T,MPTTEntry[T]]()
    MPTT(hashMap.toMap)
  }
}