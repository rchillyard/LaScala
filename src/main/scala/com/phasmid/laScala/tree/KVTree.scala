package com.phasmid.laScala.tree

import com.phasmid.laScala._
import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.fp.{Spy}

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Trait which models the tree-like aspects of a tree
  *
  * @tparam V the underlying type of the tree/node values
  */
abstract class KVTree[K,+V]()(implicit ko: KeyOps[K,V]) extends Branch[V] {

  override implicit val logger = Spy.getLogger(getClass)

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
    val yo = n.get map {case v  => ko.asInstanceOf[KeyOps[K,B]].getKeyFromValue(v)} // CHECK
    Kleenean(map2(xo,yo)(_ == _))
  }

//  /**
//    * Find a suitable parent for a node to be added to this tree.
//    *
//    * This implementation returns the parent according to getParentKey and findByKey.
//    *
//    * @param node the node to be added
//    * @tparam B the underlying type of the node
//    * @return a (optional) node which would be a suitable parent for node
//    */
//  override def findParent[B >: Value[V] : KeyOps](node: Node[B])(implicit hp: HasParent[B]): Option[Node[B]] = node.get match {
//    case Some(v) =>
//      for (k <- implicitly[HasParent[B]].getParentKey(v); n <- Spy.spy(s"key for $v is $k and parent node is: ", findByKey(k))) yield n
//    case _ => None
//  }

//  /**
//    * TODO Combine with addNode from Tree.scala
//    *
//    * @param node
//    * @param allowRecursion
//    * @tparam B
//    * @return
//    */
//  override def addNode[B >: Value[V] : TreeBuilder : NodeParent : HasParent](node: Node[B], allowRecursion: Boolean = true): Tree[B] = {
//    // get (optional) the parent node
//    val no = findParent(node)
//
//    // now, based on the parent node (if it exists), we evaluate the (potentially updated) tree and the optional parent
//    val (tree, po) = no match {
//      case Some(_) => (this, no) // Got a parent: return this as tree and optional parent
//      case _ => node.get match {
//        case Some(v) =>
//          if (allowRecursion) {
//            // Didn't get a parent: try to create one
//            Spy.spy(s"addNode: createValueFromKey (with recursion) for value: $v for $node in tree $this", implicitly[HasParent[B]].createValueFromKey(v)) match {
//              // NOTE: the following line involves a recursive call -- take care!
//              case qo@Some(p) => (addNode(p, false), qo) // created one... update the tree and return it with the new (optional) parent
//              case _ => (this, None) // failed to create one... return this with none
//            }
//          }
//          else Spy.spy(s"addNode: no parent but recursion not allowed for for $node in tree $this", (this, None))
//        case _ => (this,None)
//      }
//    }
//    val (x, y: Tree[B]) = po orElse Some(tree) match {
//        // CONSIDER define appropriate unapply methods so that we can use extractors here
//      case Some(n: Tree[B]) => (n, implicitly[TreeBuilder[B]].buildTree(n.get, n.children :+ node))
//      case Some(n: Node[B]) => (n, implicitly[TreeBuilder[B]].buildTree(n.get, Seq(node)))
//      case _ => throw TreeException(s"cannot find parent for $node or parent is not of type Tree")
//    }
//
//    // Replace node x with node y where x's value matches y's value
//    // XXX check that the cast following is justified
//    tree.replaceNode(x, y)(_.get == _.get).asInstanceOf[Tree[B]]
//  }

//  def findByKey[W >: V](k: String): Option[Node[Value[W]]] =
//    find { n: Node[Value[W]] =>
//      n.get match {
//        case Some(v) => k == v.key
//        case None => false
//    }
//  }

  def asTree[W >: V](n: Node[W])(implicit treeBuilder: TreeBuilder[W]): KVTree[K,W] = n match {
    case t: KVTree[K,W] => t
    case _ => treeBuilder.buildTree(n.get, Seq()).asInstanceOf[KVTree[K,W]]
    }
}

///**
//  * Trait to represent the fact that this object has a key.
//  * Related to, but not to be confused with, HasKey.
//  * The latter interface is used to create a type class which
//  * is then used (as a Context Bound) in Value
//  *
//  * Tried to make this based on a parametric type -- but too complex. See HasKey.
//  *
//  */
//trait WithKey[V] {
//  def key[K,W>:V](implicit ko: KeyOps[K,W]): K
//}

//trait ValueBuilder[V] {
//  /**
//    * Build a Value from just the key. The value attributes will be nulls.
//    *
//    * CONSIDER having the attributes be Option values (but that will require quite a bit of work at this point)
//    *
//    * @param k the key component of the value alone
//    * @return an appropriate Value
//    */
//  def buildValue(k: String): Value[V]
//}
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

//case class MutableGenericIndexedTreeWithKey[V](var lIndex: Option[Long], var rIndex: Option[Long], var value: Option[Value[V]], var children: Seq[Node[Value[V]]]) extends Branch[Value[V]] with IndexedNode[Value[V]] with WithKey with IndexedNodeWithKey[Value[V]] {
//  def get: Option[Value[V]] = value
//
//  def key: String = value match {
//    case Some(v) => v.key
//    case None => throw TreeException(s"logic error: this type of tree must have value in all nodes")
//  }
//}

///**
//  * Class to represent a value which is potentially at a node in a tree.
//  *
//  * Please note carefully that HasKey and WithKey are not the same and, in particular,
//  * have different definitions for the type of the key.
//  *
//  * @param value the value itself
//  * @tparam V the type of the value
//  */
//case class Value[+V](value: V) extends WithKey[V] with Renderable {
//  def key[K,W>:V](implicit ko: KeyOps[K,W]): K = ko.getKey(value)
//
//  def render(indent: Int): String = s"${Renderable.prefix(indent)}$value..."
//
//  override def toString = s"$value"
//}

object KVTree

/**
  * CONSIDER do we actually need this trait?
  *
  * @tparam V
  */
abstract class GeneralKVTreeBuilder[K,V](implicit ko: KeyOps[K,V]) extends TreeBuilder[V] {
  implicit val logger = Spy.getLogger(getClass)

  /**
    * This method determines if the two given nodes are structurally the same
    *
    * @param x node x
    * @param y node y
    * @return true if they are the same
    */
  def nodesAlike(x: Node[V], y: Node[V]): Boolean = x match {
    case b @ Branch(_, _) => Spy.spy(s"branch nodesAlike $b $y",(b like y).toBoolean(false))
    case AbstractLeaf(a) => Spy.spy(s"leaf nodesAlike $a $y",y.get contains a)
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

//case class EmptyWithKeyAndIndex[V]() extends AbstractEmpty with IndexedNodeWithKey[Value[V]] {
//  def lIndex: Option[Long] = None
//  def rIndex: Option[Long] = None
//
//  def key: String = null.asInstanceOf[String]
//}
