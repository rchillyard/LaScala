package com.phasmid.laScala.tree

import com.phasmid.laScala._
import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.fp.{HasKey, Spy}

import scala.language.implicitConversions

/**
  * Trait which models the tree-like aspects of a tree
  *
  * @tparam V the underlying type of the tree/node values
  */
sealed trait KVTree[+V] extends Branch[Value[V]] {

  /**
    * Method to determine if this Node's value is like node n's value WITHOUT any recursion.
    * NOTE: This implementation uses the key for comparison, not the value.
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  override def compareValues[B >: Value[V]](n: Node[B]): Maybe = {
    val xo = get map (v => v.key)
    val yo = n.get map {case v @ Value(_) => v.key}
    Kleenean(map2(xo,yo)(_ == _))
  }

  /**
    * Find a suitable parent for a node to be added to this tree.
    *
    * This implementation returns the parent according to getParentKey and findByKey.
    *
    * @param node the node to be added
    * @tparam B the underlying type of the node
    * @return a (optional) node which would be a suitable parent for node
    */
  override def findParent[B >: Value[V] : NodeParent](node: Node[B])(implicit hp: HasParent[B]): Option[Node[B]] = node.get match {
    case Some(v) =>
      for (k <- implicitly[HasParent[B]].getParentKey(v); n <- Spy.spy(s"key for $v is $k and parent node is: ", findByKey(k))) yield n
    case _ => None
  }

  override def addNode[B >: Value[V] : TreeBuilder : NodeParent : HasParent](node: Node[B], allowRecursion: Boolean = true): Tree[B] = {
    // get (optional) the parent node
    val no = findParent(node)

    // now, based on the parent node (if it exists), we evaluate the (potentially updated) tree and the optional parent
    val (tree, po) = no match {
      case Some(_) => (this, no) // Got a parent: return this as tree and optional parent
      case _ => node.get match {
        case Some(v) =>
          if (allowRecursion) {
            // Didn't get a parent: try to create one
            Spy.spy(s"addNode: createParent (with recursion) for value: $v for $node in tree $this", implicitly[HasParent[B]].createParent(v)) match {
              // NOTE: the following line involves a recursive call -- take care!
              case qo@Some(p) => (addNode(p, false), qo) // created one... update the tree and return it with the new (optional) parent
              case _ => (this, None) // failed to create one... return this with none
            }
          }
          else Spy.spy(s"addNode: no parent but recursion not allowed for for $node in tree $this", (this, None))
        case _ => (this,None)
      }
    }
    val (x, y: Tree[B]) = po orElse Some(tree) match {
        // CONSIDER define appropriate unapply methods so that we can use extractors here
      case Some(n: Tree[B]) => (n, implicitly[TreeBuilder[B]].buildTree(n.get, n.children :+ node))
      case Some(n: Node[B]) => (n, implicitly[TreeBuilder[B]].buildTree(n.get, Seq(node)))
      case _ => throw TreeException(s"cannot find parent for $node or parent is not of type Tree")
    }

    // Replace node x with node y where x's value matches y's value
    // XXX check that the cast following is justified
    tree.replaceNode(x, y)(_.get == _.get).asInstanceOf[Tree[B]]
  }

  def findByKey[W >: V](k: String): Option[Node[Value[W]]] =
    find { n: Node[Value[W]] =>
      n.get match {
        case Some(v) => k == v.key
        case None => false
    }
  }

  def asTree[W >: V](n: Node[Value[W]])(implicit treeBuilder: TreeBuilder[Value[W]]): KVTree[W] = n match {
    case t: KVTree[W] => t
    case _ => treeBuilder.buildTree(n.get, Seq()).asInstanceOf[KVTree[W]]
    }
}

/**
  * Trait to represent the fact that this object has a key.
  * Related to, but not to be confused with, HasKey.
  * The latter interface is used to create a type class which
  * is then used (as a Context Bound) in Value
  *
  * Tried to make this based on a parametric type -- but too complex. See HasKey.
  *
  */
trait WithKey {
  def key: String
}

trait IndexedNodeWithKey[V] extends IndexedNode[Value[V]] with WithKey

trait ValueBuilder[V] {
  /**
    * Build a Value from just the key. The value attributes will be nulls.
    *
    * CONSIDER having the attributes be Option values (but that will require quite a bit of work at this point)
    *
    * @param k the key component of the value alone
    * @return an appropriate Value
    */
  def buildValue(k: String): Value[V]
}
/**
  * A general branch of a KV-tree, where there is a value at the node itself and the number of children is unbounded.
  * Parallel to GeneralTree
  *
  * @param value    the value of the branch
  * @param children the children of this Node
  * @tparam V the underlying value type of this GeneralTree
  */
case class GeneralKVTree[V](value: Option[Value[V]], children: Seq[Node[Value[V]]]) extends KVTree[V] {
  /**
    * @return (optional) value
    */
  def get: Option[Value[V]] = value
}

case class MutableGenericIndexedTreeWithKey[V](var lIndex: Option[Long], var rIndex: Option[Long], var value: Option[Value[V]], var children: Seq[Node[Value[V]]]) extends Branch[Value[V]] with IndexedNode[Value[V]] with WithKey with IndexedNodeWithKey[V] {
  def get = value

  def key: String = value match {
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
  * @tparam V the type of the value
  */
case class Value[+V: HasKey](value: V) extends WithKey with Renderable {
  def key: String = implicitly[HasKey[V]].getKey(value).toString

  def render(indent: Int): String = s"${Renderable.prefix(indent)}$key..."

  override def toString = s"$key->$value"
}

object KVTree {
  /**
    * XXX: this method is NOT tail-recursive
    *
    * CONSIDER: not sure we really need this method since it behaves exactly like the one in Tree
    *
    * @param node  the node representing the tree to be indexed
    * @param index the starting value of index
    * @tparam V the underlying value type
    * @return an IndexedNode[Value[K,V]
    *
    *         TODO figure out why we can't actually use IndexedNode as return type
    */
  def createIndexedTree[V](node: Node[Value[V]], index: Int = 0): IndexedNodeWithKey[V] = node match {
      // CONSIDER refactoring to avoid two asInstanceOf
    case Leaf(x) => IndexedLeafWithKey[V](Some(index), Some(index + 1), x)
    case UnvaluedBinaryTree(l, r) =>
      val rIndex = index + 1 + l.size + r.size
      // XXX This is not tail-recursive
      MutableGenericIndexedTreeWithKey(Some(index), Some(rIndex), None, Seq(createIndexedTree(l, index), createIndexedTree(r, index + 1 + l.size))).asInstanceOf[IndexedNodeWithKey[V]]
    case GeneralTree(v, children) =>
      // TODO do this without using a mutable state
      var lIndex = index
      val indexedChildren = for (n <- children; s = n.size) yield { val childIndex = lIndex; lIndex += s; createIndexedTree(n, childIndex) }
      val rIndex = lIndex
      // XXX This is not tail-recursive
      MutableGenericIndexedTreeWithKey(Some(index), Some(rIndex), Some(v), indexedChildren).asInstanceOf[IndexedNodeWithKey[V]]
    case GeneralKVTree(vo, children) =>
      // TODO combine with GeneralTree case (or eliminate the GeneralTree case)
      var lIndex = index
      val indexedChildren = for (n <- children; s = n.size) yield { val childIndex = lIndex; lIndex += s; createIndexedTree(n, childIndex) }
      val rIndex = lIndex
      // XXX This is not tail-recursive
      MutableGenericIndexedTreeWithKey(Some(index), Some(rIndex), vo, indexedChildren).asInstanceOf[IndexedNodeWithKey[V]]
    case Empty => EmptyWithKeyAndIndex[V]()
    case EmptyWithIndex => EmptyWithKeyAndIndex[V]()
    case _ => throw TreeException(s"can't created IndexedTree from $node")
  }
}

trait GeneralKVTreeBuilder[V] extends TreeBuilder[Value[V]] {
  /**
    * Build a new tree, given a value and child nodes
    *
    * @param maybeValue the (optional) value which the new tree will have at its root
    * @param children   the the children of the node
    * @return a tree the (optional) value at the root and children as the immediate descendants
    */
  def buildTree(maybeValue: Option[Value[V]], children: Seq[Node[Value[V]]]): Tree[Value[V]] = GeneralKVTree(maybeValue, children)

  /**
    * Build a new leaf for a GeneralKVTree
    *
    * @param a the value for the leaf
    * @return a node which is a leaf node
    */
  def buildLeaf(a: Value[V]): Node[Value[V]] = Leaf(a)
}

object GeneralKVTree

case class IndexedLeafWithKey[V](lIndex: Option[Long], rIndex: Option[Long], value: Value[V]) extends AbstractLeaf[Value[V]](value) with IndexedNodeWithKey[V] {
  override def depth: Int = 1

  def render(indent: Int): String = value.render(indent)
  override def toString = s"""L("$value")"""

  def key: String = value.key
}

case class EmptyWithKeyAndIndex[V]() extends AbstractEmpty with IndexedNodeWithKey[V] {
  def lIndex: Option[Long] = None
  def rIndex: Option[Long] = None

  def key: String = null.asInstanceOf[String]
}
