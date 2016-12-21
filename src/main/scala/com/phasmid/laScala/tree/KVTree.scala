package com.phasmid.laScala.tree

import com.phasmid.laScala._
import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.fp.Spy
import org.slf4j.Logger

import scala.language.implicitConversions

/**
  * Trait which models the tree-like aspects of a tree
  *
  * @tparam V the underlying type of the tree/node values
  */
abstract class KVTree[K,+V]()(implicit ko: KeyOps[K,V]) extends Branch[V] {

  private implicit val logger = Spy.getLogger(getClass)

  /**
    * Method to determine if this Node's value is like node n's value WITHOUT any recursion.
    * NOTE: This implementation uses the key for comparison, not the value.
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  override def compareValues[B >: V](n: Node[B]): Maybe = {
    val xo = get map (v => ko.getKeyFromValue(v))
    val yo = n.get map (v => ko.asInstanceOf[KeyOps[K, B]].getKeyFromValue(v)) // CHECK
    Kleenean(map2(xo,yo)(_ == _))
  }

  def asTree[W >: V](n: Node[W])(implicit treeBuilder: TreeBuilder[W]): KVTree[K,W] = n match {
    case t: KVTree[K,W] => t
    case _ => treeBuilder.buildTree(n.get, Seq()).asInstanceOf[KVTree[K,W]]
    }
}

/**
  * A general branch of a KV-tree, where there is a value at the node itself and the number of children is unbounded.
  * Parallel to GeneralTree
  *
  * @param value    the value of the branch
  * @param children the children of this Node
  * @tparam V the underlying value type of this GeneralTree
  */
case class GeneralKVTree[K,V](value: Option[V], children: Seq[Node[V]])(implicit ko: KeyOps[K,V]) extends KVTree[K,V] {
  /**
    * @return (optional) value
    */
  def get: Option[V] = value
}

object KVTree

/**
  * Base class implementing TreeBuilder for GeneralKVTree
  *
  * @param ko implicit KeyOps
  * @tparam K key type
  * @tparam V value type
  */
abstract class GeneralKVTreeBuilder[K,V](implicit ko: KeyOps[K,V]) extends TreeBuilder[V] {
  private implicit val logger = Spy.getLogger(getClass)

  /**
    * This method determines if the two given nodes are structurally the same
    *
    * @param x node x
    * @param y node y
    * @return true if they are the same
    */
  def nodesAlike(x: Node[V], y: Node[V]): Boolean = x match {
    case b @ Branch(_, _) => Spy.spy(s"branch nodesAlike $b and $y",(b like y).toBoolean(false))
    case AbstractLeaf(a) => Spy.spy(s"leaf nodesAlike $x and $y",y.get contains a)
    case _ => x == y
  }

  /**
    * Get a node from an existing tree to which a new node with value a can be attached
    *
    * @param tree the tree whence we want to find the (potential) parent of a new node with value a
    * @param a    the value of the new node
    * @return the Node to which the new node will be attached (if such a node exists). Note that this might be a leaf
    */
  def getParent(tree: Tree[V], a: V): Option[Node[V]] =
    // XXX: the following is somewhat ugly but it is necessary to explicitly pass the ko parameter
    for (k <- ko.getParentKey(a); n <- tree.findByKey(k)(ko)) yield n

  /**
    * Build a new tree, given a value and child nodes
    *
    * @param maybeValue the (optional) value which the new tree will have at its root
    * @param children   the the children of the node
    * @return a tree the (optional) value at the root and children as the immediate descendants
    */
  def buildTree(maybeValue: Option[V], children: Seq[Node[V]]): Tree[V] = GeneralKVTree(maybeValue, children)

  /**
    * Build a new leaf for a GeneralKVTree
    *
    * @param a the value for the leaf
    * @return a node which is a leaf node
    */
  def buildLeaf(a: V): Node[V] = Leaf(a)
}

object GeneralKVTree
