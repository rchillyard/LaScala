package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.fp.HasKey
import com.phasmid.laScala._

import scala.language.implicitConversions

/**
  * Trait which models the tree-like aspects of a tree
  *
  * @tparam K the underlying type of the tree/node keys
  * @tparam V the underlying type of the tree/node values
  */
sealed trait KVTree[K,+V] extends TreeLike[Value[K,V]] {

//  /**
//    * Method to add a value to this tree: because the addition of values is not order-dependent this method simply invokes :+
//    *
//    * @param value       the value to add
//    * @param treeBuilder the tree builder (implicit)
//    * @param leafBuilder the leaf builder (implicit)
//    * @tparam B the underlying type of the new node (and the resulting tree)
//    * @return the resulting tree
//    */
//  def +:[B >: V](value: Value[K, B])(implicit treeBuilder: TreeBuilder[B], leafBuilder: LeafBuilder[B]): TreeLike[B] = super.+:(value)
//
//  /**
//    * Method to add a value to this tree by simply creating a leaf and calling :+(Node[B])
//    *
//    * @param value       the value to add
//    * @param treeBuilder the tree builder (implicit)
//    * @param leafBuilder the leaf builder (implicit)
//    * @tparam B the underlying type of the new node (and the resulting tree)
//    * @return the resulting tree
//    */
//  def :+[B >: V](value: Value[K, B])(implicit treeBuilder: TreeBuilder[B], leafBuilder: LeafBuilder[B]): TreeLike[B] = :+(value)
//
  /**
    * Method to determine if this Node's value is like node n's value WITHOUT any recursion.
    * NOTE: This implementation uses the key for comparison, not the value.
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  override def compareValues[B >: Value[K, V]](n: Node[B]): Maybe = {
    val xo = get map (v => v.key)
    val yo = n.get map {case v @ Value(_) => v.key}
    Kleenean(map2(xo,yo)(_ == _))
  }
}

/**
  * Trait to represent the fact that this object has a key.
  * Related to, but not to be confused with, HasKey.
  * The latter interface is used to create a type class which
  * is then used (as a Context Bound) in Value
  *
  * @tparam K the type of the key
  */
trait WithKey[K] {
  def key: K
}

trait IndexedNodeWithKey[K,V] extends IndexedNode[Value[K,V]] with WithKey[K]

case class MutableGenericIndexedTreeWithKey[K,V](var lIndex: Option[Long], var rIndex: Option[Long], var value: Option[Value[K,V]], var children: Seq[Node[Value[K,V]]]) extends Branch[Value[K,V]] with IndexedNode[Value[K,V]] with WithKey[K] with IndexedNodeWithKey[K,V] {
  def get = value

  def key: K = value match {
    case Some(v) => v.key
    case None => throw TreeException(s"logic error: this type of tree must have value in all nodes")
  }
}

/**
  * Class to represent a value which is potentially at a node in a tree.
  *
  * Please note carefully that HasKey and WithKey are not the same and, in particular,
  * have different definitions for the type of the key.
  *
  * @param value the value itself
  * @tparam K the type of the key
  * @tparam V the type of the value
  */
case class Value[K, +V : HasKey](value: V) extends WithKey[K] with Renderable {
  // NOTE we are explicitly casting the result of method key(value), which has type HasKey#K (that's to say
  // a parametric type defined by implementations of HasKey), to K which is declared independently for this Value type.
  def key: K = implicitly[HasKey[V]].getKey(value).asInstanceOf[K]

  def render(indent: Int): String = s"${Renderable.prefix(indent)}${key}..."

  override def toString = s"$key->$value"
}

object KVTree {
  def populateGeneralTree[K,V](values: Seq[Value[K,V]])(implicit treeBuilder: TreeBuilder[Value[K,V]], leafBuilder: LeafBuilder[Value[K,V]]): TreeLike[Value[K,V]] = {
    values match {
      case h :: t =>
        var result: TreeLike[Value[K,V]] = implicitly[TreeBuilder[Value[K,V]]].buildTree(implicitly[LeafBuilder[Value[K,V]]].buildLeaf(h), Empty)
        for (w <- t) {
          result = result :+ Leaf(w)
        }
        result
    }
  }

  /**
    * XXX: this method is NOT tail-recursive
    *
    * CONSIDER: not sure we really need this method since it behaves exactly like the one in Tree
    *
    * @param node  the node representing the tree to be indexed
    * @param index the starting value of index
    * @tparam K the underlying key type
    * @tparam V the underlying value type
    * @return an IndexedNode[Value[K,V]
    *
    *         TODO figure out why we can't actually use IndexedNode as return type
    */
  def createIndexedTree[K,V](node: Node[Value[K,V]], index: Int = 0): IndexedNodeWithKey[K,V] = node match {
      // CONSIDER refactoring to avoid two asInstanceOf
    case Leaf(x) => IndexedLeafWithKey[K,V](Some(index), Some(index + 1), x)
    case UnvaluedBinaryTree(l, r) =>
      val rIndex = index + 1 + l.size + r.size
      // XXX This is not tail-recursive
      MutableGenericIndexedTreeWithKey(Some(index), Some(rIndex), None, Seq(createIndexedTree(l, index), createIndexedTree(r, index + 1 + l.size))).asInstanceOf[IndexedNodeWithKey[K,V]]
    case GeneralTree(v, children) =>
      // TODO do this without using a mutable state
      var lIndex = index
      val indexedChildren = for (n <- children; s = n.size) yield { val childIndex = lIndex; lIndex += s; createIndexedTree(n, childIndex) }
      val rIndex = lIndex
      // XXX This is not tail-recursive
      MutableGenericIndexedTreeWithKey(Some(index), Some(rIndex), Some(v), indexedChildren).asInstanceOf[IndexedNodeWithKey[K,V]]
    case Empty => EmptyWithKeyAndIndex[K,V]()
    case EmptyWithIndex => EmptyWithKeyAndIndex[K,V]()
    case _ => throw TreeException(s"can't created IndexedTree from $node")
  }
}

case class IndexedLeafWithKey[K,V](lIndex: Option[Long], rIndex: Option[Long], value: Value[K,V]) extends AbstractLeaf[Value[K,V]](value) with IndexedNodeWithKey[K,V] {
  override def depth: Int = 1
  def render(indent: Int) = value.render(indent)
  override def toString = s"""L("$value")"""
  def key: K = value.key
}
case class EmptyWithKeyAndIndex[K,V]() extends AbstractEmpty with IndexedNodeWithKey[K,V] {
  def lIndex: Option[Long] = None
  def rIndex: Option[Long] = None
  def key: K = null.asInstanceOf[K]
}