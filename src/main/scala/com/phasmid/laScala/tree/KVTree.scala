package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.fp.{FP, HasKey, Spy}
import com.phasmid.laScala._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Trait which models the tree-like aspects of a tree
  *
  * @tparam K the underlying type of the tree/node keys
  * @tparam V the underlying type of the tree/node values
  */
sealed trait KVTree[K,+V] extends Branch[Value[K,V]] {

  //  iterator(true).mkString(", ")

  //  renderRecursive[A]((ns, n) => n
  //  match {
  //    case Branch(x) => x ++ ns
  //    case _ => ns
  //  })



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

  def findByKey[W >: V](k: K): Option[Node[Value[K,W]]] =
    find{n: Node[Value[K, W]] => n.get match {
      case Some(v) => k == v.key
      case None => false
    }
  }

//  def attachNode[W >: V](value: Value[K, W])(implicit ev: HasParent[K,Value[K, W]], treeBuilder: TreeBuilder[Value[K, W]], leafBuilder: LeafBuilder[Value[K, W]], valueBuilder: ValueBuilder[K,W]): Try[KVTree[K, W]] = {
//    val parentKey = implicitly[HasParent[K, Value[K, W]]].getParent(value)
//    val po = find{n: Node[Value[K, W]] => n.get match {
//      case Some(v) => parentKey == v.key
//      case None => false
//    }}
//    Spy.spying = true
//    val pt0 = FP.optionToTry(po)
//    val pt1 = pt0.recoverWith{case x =>
//      // FIXME here we need to join p to the tree, with root as parent if none found
//      for (p <- Try(leafBuilder.buildLeaf(Some(valueBuilder.buildValue(parentKey))))) yield p}
//    pt1 match {
//      case Success(p) => Success(Spy.spy("attached",(asTree(p) :+ leafBuilder.buildLeaf(value)).asInstanceOf[KVTree[K,V]]))
//      case Failure(x) => Failure(TreeException(s"unable to find or create parent $parentKey in order to attach node for $value: ${x.getLocalizedMessage}"))
//    }
//  }

    def asTree[W >: V](n: Node[Value[K,W]])(implicit treeBuilder: TreeBuilder[Value[K, W]]): KVTree[K, W] = n match {
      case t: KVTree[K,W] => t
      case _ => treeBuilder.buildTree(n.get,Seq()).asInstanceOf[KVTree[K,W]]
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

trait ValueBuilder[K,V] {
  /**
    * Build a Value from just the key. The value attributes will be nulls.
    *
    * CONSIDER having the attributes be Option values (but that will require quite a bit of work at this point)
    *
    * @param k the key component of the value alone
    * @return an appropriate Value
    */
  def buildValue(k: K): Value[K,V]
}
/**
  * A general branch of a KV-tree, where there is a value at the node itself and the number of children is unbounded.
  * Parallel to GeneralTree
  *
  * @param value    the value of the branch
  * @param children the children of this Node
  * @tparam K the underlying key type of this GeneralTree
  * @tparam V the underlying value type of this GeneralTree
  */
case class GeneralKVTree[K,V](value: Value[K,V], children: Seq[Node[Value[K,V]]]) extends KVTree[K,V] {
  /**
    * @return Some(value)
    */
  def get = Some(value)
}



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
//  /**
//    * This implementation of populateGeneralKVTree takes a sequence of Values.
//    *
//    * @param values
//    * @param treeBuilder
//    * @param leafBuilder
//    * @tparam K
//    * @tparam V
//    * @return
//    */
//  def populateGeneralKVTree[K,V](values: Seq[Value[K,V]])(implicit treeBuilder: TreeBuilder[Value[K,V]], leafBuilder: LeafBuilder[Value[K,V]]): KVTree[K,V] = {
//    values match {
//      case h :: t =>
//        var tree: KVTree[K,V] = implicitly[TreeBuilder[Value[K,V]]].buildTree(implicitly[LeafBuilder[Value[K,V]]].buildLeaf(h), Empty).asInstanceOf[KVTree[K,V]]
//        for (w <- t) {
//          tree = (tree :+ Leaf(w)).asInstanceOf[KVTree[K,V]]
//        }
//        tree
//    }
//  }

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
    case GeneralKVTree(v, children) =>
      // TODO combine with GeneralTree case (or eliminate the GeneralTree case)
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

trait GeneralKVTreeBuilder[K,V] extends TreeBuilder[Value[K,V]] {
// TODO: this isn't right: we need to find where to add the node
//  def buildTree[W >: V](tree:  Node[Value[K,W]], node: Node[Value[K,W]])(implicit ev: HasParent[K, Value[K, W]], leafBuilder: LeafBuilder[Value[K,W]], valueBuilder: ValueBuilder[K,W]): TreeLike[Value[K,W]] = {
//    require(tree.isInstanceOf[KVTree[K,W]],"tree must be an instance of KVTree")
//    val t = tree.asInstanceOf[KVTree[K,W]]
//    val h = implicitly[HasParent[K, Value[K, W]]]
//    val ko = node.get map (h.getParent(_))
//    val vno = for (k <- ko; vn <- t.findByKey(k)) yield vn
//    Spy.spying = true
//    val pt0 = FP.optionToTry(vno)
//    val pt1 = pt0.recoverWith{case x =>
//      // FIXME here we need to join p to the tree, with root as parent if none found
//      // FIXME don't use ko.get
//      for (p <- Try(leafBuilder.buildLeaf(valueBuilder.buildValue(ko.get)))) yield p}
//
//    tree match {
//      case GeneralKVTree(v, ns) => GeneralKVTree(v, ns :+ node)
//      case Leaf(a) => GeneralKVTree(a, Seq(node))
//      case _ => throw TreeException(s"not implemented: $tree")
//    }
//  }
  /**
    * Build a new tree, given a value and child nodes
    *
    * @param maybeValue the (optional) value which the new tree will have at its root
    * @param children   the the children of the node
    * @return a tree the (optional) value at the root and children as the immediate descendants
    */
  def buildTree(maybeValue: Option[Value[K, V]], children: Seq[Node[Value[K, V]]]): TreeLike[Value[K, V]] =
  // TODO avoid use of get
  GeneralKVTree(maybeValue.get,children)
}
trait GeneralKVLeafBuilder[K,V] extends LeafBuilder[Value[K,V]] {
  def buildLeaf(ao: Option[Value[K, V]]): Node[Value[K, V]] = (ao map (Leaf(_)) orElse(Some(Empty))).get
}

object GeneralKVTree

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
