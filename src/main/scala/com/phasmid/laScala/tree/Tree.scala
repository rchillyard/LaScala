package com.phasmid.laScala.tree

import com.phasmid.laScala.Recursion
import com.phasmid.laScala.fp.FP._

/**
  * This trait expresses the notion of a Node from a tree.
  *
  * NOTE: this is all experimental and I apologize for the very poor quality of code in this module.
  *
  * Created by scalaprof on 10/19/16.
  */
sealed trait Node[+A] {
  /**
    * Optionally get the value of this Node.
    *
    * @return If this node contains an actual value x, return Some(x) else None
    */
  def get: Option[A]

  /**
    * Get the children of this Node, if any.
    * @return the children as a Seq of Nodes
    */
  def children: Seq[Node[A]]

  /**
    * Calculate the size of this node's subtree
    *
    * @return the size of the subtree
    */
  def size: Int

  /**
    * Calculate the depth of this node's subtree
    *
    * @return the depth of the subtree
    */
  def depth: Int

  /**
    * Test whether this Node contains a value or children
    *
    * @return true if node contains value or children; otherwise false
    */
  def isEmpty: Boolean

  /**
    * Create a String which represents this Node and its subtree
    *
    * @return an appropriate String
    */
  def render: String

  /**
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  def like[B >: A](n: Node[B]): Boolean

  /**
    * CONSIDER moving this into TreeLike
    *
    * @param n the node to be searched
    * @tparam B the type of n
    * @return true if node n is found in the sub-tree defined by this
    */
  def includes[B >: A](n: Node[B]): Boolean

  /**
    * CONSIDER moving this into TreeLike
    *
    * @param b the value to be searched
    * @tparam B the type of b
    * @return true if value b is found in the sub-tree defined by this
    */
  def includes[B >: A](b: B): Boolean

  /**
    * CONSIDER moving this into TreeLike
    *
    * @param p the predicate to be applied to the value of each node in turn (depth first)
    * @tparam B the type of b
    * @return Some(node) where node is the first node to be found that satisfies the predicate p
    */
  def find[B >: A](p: Node[B]=>Boolean): Option[Node[B]]
}

/**
  * Trait which models an index that is useful for building an indexed tree, especially an MPTT-type index.
  * Note that we only add one index here -- for MPTT, we need to pass through the tree in both directions to get the left and right indices.
  */
trait TreeIndex {
  def lIndex: Option[Long]
  def rIndex: Option[Long]
}

trait IndexedNode[A] extends Node[A] with TreeIndex


/**
  * This trait expresses the notion of a tree-like structure.
  *
  * Created by scalaprof on 10/19/16.
  */
sealed trait TreeLike[+A] extends Node[A]{

  /**
    * Method to safely cast Node n to a TreeLike object.
    * Note: This is not an instance method in that it does not reference this
    *
    * @param n  the node to cast
    * @param treeMaker implicit implementation of TreeMaker trait
    * @tparam B the underlying type of node n
    * @return if n is already a TreeLike object then return it, otherwise return a sub-tree based on n
    */
  def asTree[B >: A : Ordering](n: Node[B])(implicit treeMaker: TreeMaker): TreeLike[B] = n match {
    case t: TreeLike[B] => t
    case _ => treeMaker.tree(n)
  }

  /**
    * Method to combine this tree with Node b (where the tree is on the left of the operator)
    *
    * @param b the node to be added to this tree
    * @tparam B the underlying type of b
    * @return a new subtree of type B, as a Node[B]
    */
  def :+[B >: A : Ordering](b: Node[B]): Node[B]

  /**
    * Method to combine this tree with Node b (where the tree is on the right of the operator)
    *
    * @param b the node to be added to this tree
    * @param treeMaker implicit implementation of TreeMaker trait
    * @tparam B the underlying type of node n
    * @return if n is already a TreeLike object then return it, otherwise return a sub-tree based on n
    */
  def +:[B >: A : Ordering](b: Node[B])(implicit treeMaker: TreeMaker): Node[B] = asTree(b) :+ this

  def size: Int = Parent.traverse[Node[A], Int, Int]({
    case Empty => 0
    case _ => 1
  }, _+_, { x => false})(List(this), 0)

  def includes[B >: A](node: Node[B]): Boolean = Parent.traverse[Node[B], Boolean, Boolean]({ n => n like node}, _|_, {x => x})(List(this), false)

  def includes[B >: A](b: B): Boolean = Parent.traverse[Node[B], Boolean, Boolean]({ n => n.get match {
    case Some(`b`) => true
    case _ => false
  }}, _|_, {x => x})(List(this), false)

  def find[B >: A](p: Node[B]=>Boolean): Option[Node[B]] = {
    def f(n: Node[B]): Option[Node[B]] = toOption(p)(n)
    def g(bno: Option[Node[B]], t: Option[Node[B]]): Option[Node[B]] = bno orElse t
    def q(bno: Option[Node[B]]): Boolean = bno.isDefined
    Parent.traverse[Node[B], Option[Node[B]], Option[Node[B]]](f, g, q)(List(this), None)
  }

  def renderRecursive[B >: A](z: (List[Node[B]], Node[B]) => List[Node[B]]): String =
    Recursion.recurse[Node[B], String, String]({
      case Empty => ""
      case Leaf(x) => x.toString
      case c@Open => c.render
      case c@Close => c.render
      case n => ""
    }, _ + _, z)(List(this), "")

  override def render: String = renderRecursive[A]((ns,n) => n.children.toList ++ ns)

}

/**
  * This trait expresses the notion of a tree-like structure that has properties depth and size.
  *
  * Created by scalaprof on 10/19/16.
  */
sealed abstract class Tree[+A] extends TreeLike[A] {
  /**
    * NOTE: that this is NOT tail-recursive
    *
    * NOTE: I don't think we can use Parent.traverse for depth. Could be wrong.
    *
    * @return the depth of the subtree
    */
  def depth: Int = 1+children.map(_.depth).max

}

trait TreeMaker {
  def tree[T](node: Node[T]): TreeLike[T]
}

/**
  * This implements a non-indexed binary tree
  *
  * @param value the value at the root of this abstract binary tree
  * @param left the tree to the "left" (the tree which contains values which are less than value)
  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
case class BinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](value, left, right) {
  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T] = BinaryTree(x,l,r)
}

///**
//  * This implements a indexed binary tree
//  *
//  * @param value the value at the root of this abstract binary tree
//  * @param left the tree to the "left" (the tree which contains values which are less than value)
//  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
//  * @tparam A the underlying type
//  */
//case class IndexedBinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](value, left, right) with TreeIndex {
//  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T] = IndexedBinaryTree(x,l,r)
//
//  def index: Long = left match {
//    case IndexedNode(n,i) => i
//    case _ => throw new TreeException(s"found non-indexed node in IndexedBinaryTree")
//  }
//}

case class Leaf[+A](value: A)(implicit treeMaker: TreeMaker) extends TreeLike[A] {
  def children = Nil
  def get = Some(value)
  def :+[B >: A : Ordering](b: Node[B]): Node[B] = treeMaker.tree(b) :+ this
  override def depth: Int = 1
  override def render = s"""$value"""
  override def toString = s"""L("$value")"""
  def isEmpty: Boolean = false
  def like[B >: A](n: Node[B]): Boolean = n.get match { case Some(`value`) => true; case _ => false }
}



//abstract class IndexedNode[+A](n: Node[A], i: Long)(implicit treeMaker: TreeMaker) extends TreeLike[A] with TreeIndex {
//  def node = n
//
//  def index = i
//
//  def get: Option[A] = node.get
//
//  def children: Seq[Node[A]] = node.children
//
//  def depth: Int = node.depth
//
//  def isEmpty: Boolean = node.isEmpty
//}

//case class IndexedTree[A](n: Node[A], i: Long)(implicit treeMaker: TreeMaker) extends IndexedNode[A](n,i)(treeMaker) {
//  def :+[B >: A : Ordering](b: Node[B]): Node[B] = treeMaker.tree(b) :+ this
//}

object IndexedNode {
//  implicit val treeMaker = new TreeMaker {
//    def tree[T](node: Node[T]): TreeLike[T] = node match {
//      case t: IndexedBinaryTree[T] => t
//      case Leaf(x) => IndexedTree(x,Empty,Empty)
//      case _ => throw TreeException(s"cannot build BinaryTree from $node")
//    }
//  }

//  def indexTree[A : Ordering](node: Node[A]): IndexedNode[A] = {
//    def f(index: Int, node: Node[A]): IndexedNode[A] = IndexedTree(node,index)
//    def g(x: IndexedNode[A],y: Node[A]): IndexedNode[A] = (x.asInstanceOf[IndexedTree[A]] :+ y).asInstanceOf[IndexedNode[A]]
//    def h(ns: List[Node[A]], n: Node[A]): List[Node[A]] = n.children.toList ++ ns
//    val start: IndexedBinaryTree[A] = IndexedBinaryTree(Nil, 0, 0).asInstanceOf[IndexedBinaryTree[A]]
//    Recursion.countRecurse[Node[A],IndexedNode[A],IndexedNode[A],Int](f,g,h)(List(),0,start)
//  }

  def unapply[A](e: IndexedNode[A]): Option[(Node[A], Long, Long)] =
    for (x <- Option(e); l <- x.lIndex; r <- x.rIndex) yield (x, l, r)

//  def createIndex[A : Ordering](node: Node[A]): MPTT[A] = node match
//  {
//    case in @ IndexedNode(n,i) => MPTT(in)
//    case Leaf(x) => throw new TreeException(s"cannot create an index from a Leaf")
//    case Empty => throw new TreeException(s"cannot create an index from an Empty")
//    case node: Node[A] => val ev = implicitly[Ordering[A]]; createIndex(indexTree(node))(ev)
//    case _ => throw new TreeException(s"cannot create an index from $this")
//  }


}

case object Empty extends TreeLike[Nothing] {
  def children = Nil
  def get = None
  def :+[B >: Nothing : Ordering](bn: Node[B]): Node[B] = bn
  override def depth: Int = 0
  override def render = "ø"
  override def toString = "Empty"
  def isEmpty: Boolean = true
  def like[B >: Nothing](n: Node[B]): Boolean = ???
}
case object EmptyWithIndex extends TreeLike[Nothing] with TreeIndex {
  def children = Nil
  def get = None
  def :+[B >: Nothing : Ordering](bn: Node[B]): Node[B] = bn
  override def depth: Int = 0
  override def render = "ø"
  override def toString = "EmptyWIthIndex"
  def isEmpty: Boolean = true
  def lIndex: Option[Long] = None
  def rIndex: Option[Long] = None
  def like[B >: Nothing](n: Node[B]): Boolean = ???
}

/**
  * This abstract class knows how to create a binary tree
  *
  * @param value the value at the root of this abstract binary tree
  * @param left the tree to the "left" (the tree which contains values which are less than value)
  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
sealed abstract class AbstractBinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends Tree[A] {
  def get: Option[A] = Some(value)

  def children: Seq[Node[A]] = Seq(left,right)

  def like[B >: A](n: Node[B]): Boolean = ??? // liftPredicate2(_==_)(get, n.get) getOrElse(false)

  implicit val builder = new TreeMaker {
    def tree[T](node: Node[T]): TreeLike[T] = make(node.get.get,Empty,Empty)
  }

  def :+[B >: A : Ordering](n: Node[B]): Node[B] = {
    val b = implicitly[Ordering[B]].compare(value, n.get.get) < 0
    def maybeAdd(x: Node[B], y: Boolean): Node[B] = if (y) asTree(x) :+ n else x
    make(value, maybeAdd(left,!b), maybeAdd(right,b))
  }

  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T]
  def isEmpty: Boolean = false

  override def render: String = renderRecursive[A](AbstractBinaryTree.buildNodeList)

  override def toString: String = s"""BT{$showValue:$left:$right}"""

  def showValue = get match {
    case Some(x) => s""""$x""""
    case None => s"ø"
  }
}

object AbstractBinaryTree {
  def buildNodeList[T](nodes: List[Node[T]], node: Node[T]): List[Node[T]] = node match {
    case BinaryTree(x, l, r) => (expand(l) :+ Leaf(x)(node.asInstanceOf[BinaryTree[T]].builder)) ++ expand(r) ++ nodes
    case _ => node.children.toList ++ nodes
  }
  private [tree] def expand[T](node: Node[T]): List[Node[T]] = node match {
    case Empty => Nil
    case n => List(Open,n,Close)
  }
}

object Node {
  /**
    * Implicit definition of a Parent for the (type-parameterized, i.e. generic) type Node[A]
    *
    * @tparam A the underlying node type
    * @return a Parent of type Node[A]
    */
  implicit def nodeParent[A]: Parent[Node[A]] = new Parent[Node[A]] {
    /**
      * Get the children of this Node, if any.
      *
      * @return the children as a Seq of Nodes
      */
    def children(t: Node[A]): Seq[Node[A]] = t.children
  }

}

//object IndexedTree {
//  /**
//    * Method to create an indexed subtree from the given details.
//    *
//    * Note: this is recursive but NOT tail-recursive
//    *
//    * @param index the left-index that will be appropriate for this subtree
//    * @param x the value at the root of the subtree
//    * @param l the left tree
//    * @param r the right tree
//    * @tparam T the underlying type of the tree
//    * @return a tuple consisting of the new index value and the indexed subtree
//    */
//  def makeIndexedTree[T](index: Int, x: T, l: Node[T], r: Node[T]): (Int,Node[T]) = {
//    println(s"makeIndexedTree (1): $index, $x, $l, $r")
//    val (i,li) = indexSubtree(index,l)
//    println(s"makeIndexedTree (2): $i, $li")
//    val (j,ri) = indexSubtree(i+2,r)
//    println(s"makeIndexedTree (3): $j, $ri")
//    (j,IndexedTree(index,j,x,li,ri))
//  }
//
//  private [tree] def indexSubtree[T](index: Int, node: Node[T]): (Int, Node[T]) = node match {
//    case BinaryTree(v,l,r) => makeIndexedTree(index,v,l,r)
//    case IndexedTree(_, _, v, l, r) => makeIndexedTree(index,v,l,r)
//    case Leaf(v) => (index+2,IndexedLeaf(index,index+1,v))
//    case Empty => (index,node)
//    case _ => throw TreeException(s"illegal node in IndexedTree: $node")
//  }

//  implicit val treeMaker = new TreeMaker {
//    def tree[T](node: Node[T]): TreeLike[T] = indexSubtree(0,node)._2.asInstanceOf[TreeLike[T]]
//  }
//}

case class GenericTree[+A](value: A, children: Seq[Node[A]]) extends Tree[A] {
  def get = Some(value)
  def :+[B >: A : Ordering](bn: Node[B]): Node[B] = GenericTree(value,children:+bn)
  def isEmpty: Boolean = false
  def like[B >: A](n: Node[B]): Boolean = ???
}

case class MutableGenericIndexedTree[A](var lIndex: Option[Long], var rIndex: Option[Long], var value: A, var children: Seq[Node[A]]) extends Tree[A] with IndexedNode[A] {
  def get = Some(value)
  def :+[B >: A : Ordering](bn: Node[B]): Node[B] = MutableGenericIndexedTree(None,None,value,children:+bn).asInstanceOf[Node[B]]
  def isEmpty: Boolean = false
  def like[B >: A](n: Node[B]): Boolean = ???
}

case class IndexedLeaf[A](lIndex: Option[Long], rIndex: Option[Long], value: A) extends TreeLike[A] with TreeIndex {
  def children = Nil
  def get = Some(value)
  def :+[B >: A : Ordering](b: Node[B]): Node[B] = ??? // treeMaker.tree(b) :+ this
  override def depth: Int = 1
  override def render = s"""$value"""
  override def toString = s"""L("$value")"""
  def isEmpty: Boolean = false
  def like[B >: A](n: Node[B]): Boolean = ???
}

object GenericTree {
  def apply[A](as: A*): Node[A] = as.toList match {
    case Nil => Empty.asInstanceOf[Node[A]]
    case h::t =>
      apply(h,List(apply(t: _*)))
  }
  def apply[A](as: List[A]): Node[A] = apply(as:_*)
}

object BinaryTree {
  def apply[A : Ordering](as: A*): TreeLike[A] = as.toList match {
    case Nil => Empty.asInstanceOf[TreeLike[A]]
    case h::Nil => BinaryTree(h,Empty,Empty)
    case h::t => (apply(h) :+ apply(t:_*)).asInstanceOf[BinaryTree[A]]
  }
  implicit val treeMaker = new TreeMaker {
    def tree[T](node: Node[T]): TreeLike[T] = node match {
      case t: BinaryTree[T] => t
      case Leaf(x) => BinaryTree(x,Empty,Empty)
      case _ => throw TreeException(s"cannot build BinaryTree from $node")
    }
  }
}

//object IndexedBinaryTree {
//  def apply[A : Ordering](as: A*): TreeLike[A] = as.toList match {
//    case Nil => Empty.asInstanceOf[TreeLike[A]]
//    case h::Nil => IndexedBinaryTree(h,Empty,Empty)
//    case h::t => (apply(h) :+ apply(t:_*)).asInstanceOf[IndexedBinaryTree[A]]
//  }
//  implicit val treeMaker = new TreeMaker {
//    def tree[T](node: Node[T]): TreeLike[T] = node match {
//      case t: IndexedBinaryTree[T] => t
//      case Leaf(x) => IndexedBinaryTree(x,Empty,Empty)
//      case _ => throw TreeException(s"cannot build BinaryTree from $node")
//    }
//  }
//}

case class TreeException(msg: String) extends Exception(msg)

object Tree {

  def join(xo: Option[String], yo: Option[String]): Option[String] = {
    val mt = Some("")
    for (x <- xo.orElse(mt); y <- yo.orElse(mt)) yield x+y
  }

  /**
    * XXX: this method is NOT tail-recursive
    * @param node the node representing the tree to be indexed
    * @param index the starting value of index
    * @tparam A the underlying type
    * @return an IndexedNode[A]
    *
    *         TODO figure out why we can't actually use IndexedNode as return type
    */
  def createIndexedTree[A](node: Node[A], index: Int): Node[A] with TreeIndex = node match {
    case Leaf(x) => IndexedLeaf[A](Some(index),Some(index+1),x)
    case BinaryTree(v,l,r) =>
      val rIndex = index+1+l.size+r.size
      MutableGenericIndexedTree(Some(index),Some(rIndex),v,Seq(createIndexedTree(l,index),createIndexedTree(r,index+1+l.size)))
    case Empty => EmptyWithIndex
    case _ => throw TreeException(s"can't created IndexedTree from $node")
  }

}

object TreeLike {
  def populateTree(values: Seq[String]): TreeLike[String] = {
        import BinaryTree._
    values match {
      case h :: t =>
        var result = BinaryTree[String](h)
        for (w <- t) {
          result = (result :+ Leaf(w)).asInstanceOf[TreeLike[String]]
        }
        result
    }
  }
}

abstract class Punctuation(x: String) extends Node[Nothing] {
  def get: Option[Nothing] = None
  def children: Seq[Node[Nothing]] = Nil
  def includes[B >: Nothing](b: B): Boolean = false
  def includes[B >: Nothing](n: Node[B]): Boolean = false
  def size: Int = 0
  def render: String = x
  def isEmpty: Boolean = true
  def depth: Int = 0
  def find[B >: Nothing](p: Node[B]=>Boolean): Option[Node[B]] = None
  def like[B >: Nothing](n: Node[B]): Boolean = ???
}

case object Open extends Punctuation("{")
case object Close extends Punctuation("}")

