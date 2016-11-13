package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.{Kleenean, Maybe, Recursion, ^^}

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
    *
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
    * Compare the value of this node with the value of node n
    *
    * @param n the node to be compared
    * @tparam B the underlying type of node n
    * @return a Maybe based on the comparison
    */
  def compareValues[B >: A](n: Node[B]): Maybe = Kleenean(map2(get, n.get)(_ == _))

  /**
    * Method to determine if this Node is like node n WITHOUT any recursion.
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  def like[B >: A](n: Node[B]): Maybe

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
  def find[B >: A](p: Node[B] => Boolean): Option[Node[B]]
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
sealed trait TreeLike[+A] extends Node[A] {
  /**
    * Method to safely cast Node n to a TreeLike object.
    * Note: This is not an instance method in that it does not reference this
    *
    * @param n         the node to cast
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
    * @param b         the node to be added to this tree
    * @param treeMaker implicit implementation of TreeMaker trait
    * @tparam B the underlying type of node n
    * @return if n is already a TreeLike object then return it, otherwise return a sub-tree based on n
    */
  def +:[B >: A : Ordering](b: Node[B])(implicit treeMaker: TreeMaker): Node[B] = asTree(b) :+ this

  def size: Int = Parent.traverse[Node[A], Int, Int]({
    case Empty => 0
    case _ => 1
  }, _ + _, { x => false })(List(this), 0)

  /**
    * The default implementation of like simply compares the value of this node with the value of node n
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  def like[B >: A](n: Node[B]): Maybe = compareValues(n)

  def includes[B >: A](node: Node[B]): Boolean = {
    val f: Node[B] => Maybe = { n => n like node }
    val g: (Boolean, Maybe) => Boolean = { (b, m) => (b |: m) ().getOrElse(false) }
    Parent.traverse[Node[B], Maybe, Boolean](f, g, { x => x })(List(this), false)
  }

  def includes[B >: A](b: B): Boolean = Parent.traverse[Node[B], Boolean, Boolean]({ n => n.get match {
    case Some(`b`) => true
    case _ => false
  }
  }, _ | _, { x => x })(List(this), false)

  def find[B >: A](p: Node[B] => Boolean): Option[Node[B]] = {
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

  override def render: String = renderRecursive[A]((ns, n) => n.children.toList ++ ns)
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
  def depth: Int = 1 + children.map(_.depth).max
}

trait TreeMaker {
  def tree[T](node: Node[T]): TreeLike[T]
}

/**
  * This implements a non-indexed binary tree
  *
  * @param value the value at the root of this abstract binary tree
  * @param left  the tree to the "left" (the tree which contains values which are less than value)
  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
case class BinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](value, left, right) {
  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T] = BinaryTree(x, l, r)
}

/**
  * This implements a leaf node
  *
  * @param value     the value of the leaf
  * @param treeMaker an (implicit) TreeMaker
  * @tparam A the type of the value
  */
case class Leaf[+A](value: A)(implicit treeMaker: TreeMaker) extends TreeLike[A] {
  def children = Nil

  def get = Some(value)

  def :+[B >: A : Ordering](b: Node[B]): Node[B] = treeMaker.tree(b) :+ this

  override def depth: Int = 1

  override def render = s"""$value"""

  override def toString = s"""L("$value")"""

  def isEmpty: Boolean = false
}

object IndexedNode {
  def unapply[A](e: IndexedNode[A]): Option[(Node[A], Long, Long)] =
    for (x <- Option(e); l <- x.lIndex; r <- x.rIndex) yield (x, l, r)
}

case object Empty extends TreeLike[Nothing] {
  def children = Nil

  def get = None

  def :+[B >: Nothing : Ordering](bn: Node[B]): Node[B] = bn

  override def depth: Int = 0

  override def render = "ø"

  override def toString = "Empty"

  def isEmpty: Boolean = true

  override def like[B >: Nothing](n: Node[B]): Maybe = n match {
    case Empty => Kleenean(true)
    case _ => ^^
  }

  override def compareValues[B >: Nothing](n: Node[B]): Maybe = n match {
    case Empty => Kleenean(true)
    case _ => ^^
  }
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

  override def like[B >: Nothing](n: Node[B]): Maybe = n match {
    case Empty => Kleenean(true)
    case _ => ^^
  }

  override def compareValues[B >: Nothing](n: Node[B]): Maybe = n match {
    case Empty => Kleenean(true)
    case _ => ^^
  }
}

/**
  * This abstract class knows how to create a binary tree
  *
  * @param value the value at the root of this abstract binary tree
  * @param left  the tree to the "left" (the tree which contains values which are less than value)
  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
sealed abstract class AbstractBinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends Tree[A] {
  def get: Option[A] = Some(value)

  def children: Seq[Node[A]] = Seq(left, right)

  override def like[B >: A](n: Node[B]): Maybe = n match {
    case AbstractBinaryTree(v, l, r) => compareValues(n) :& left.compareValues(l) :& right.compareValues(r)
    case _ => Kleenean(false)
  }

  implicit val builder = new TreeMaker {
    def tree[T](node: Node[T]): TreeLike[T] = make(node.get.get, Empty, Empty)
  }

  def :+[B >: A : Ordering](n: Node[B]): Node[B] = {
    val b = implicitly[Ordering[B]].compare(value, n.get.get) < 0
    def maybeAdd(x: Node[B], y: Boolean): Node[B] = if (y) asTree(x) :+ n else x
    make(value, maybeAdd(left, !b), maybeAdd(right, b))
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
  def unapply[A](e: AbstractBinaryTree[A]): Option[(A, Node[A], Node[A])] =
    for (x <- e.get) yield (x, e.children.head, e.children.last)

  def buildNodeList[T](nodes: List[Node[T]], node: Node[T]): List[Node[T]] = node match {
    case BinaryTree(x, l, r) => (expand(l) :+ Leaf(x)(node.asInstanceOf[BinaryTree[T]].builder)) ++ expand(r) ++ nodes
    case _ => node.children.toList ++ nodes
  }

  private[tree] def expand[T](node: Node[T]): List[Node[T]] = node match {
    case Empty => Nil
    case n => List(Open, n, Close)
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

case class GenericTree[+A](value: A, children: Seq[Node[A]]) extends Tree[A] {
  def get = Some(value)

  def :+[B >: A : Ordering](bn: Node[B]): Node[B] = GenericTree(value, children :+ bn)

  def isEmpty: Boolean = false
}

case class MutableGenericIndexedTree[A](var lIndex: Option[Long], var rIndex: Option[Long], var value: A, var children: Seq[Node[A]]) extends Tree[A] with IndexedNode[A] {
  def get = Some(value)

  def :+[B >: A : Ordering](bn: Node[B]): Node[B] = MutableGenericIndexedTree(None, None, value, children :+ bn).asInstanceOf[Node[B]]

  def isEmpty: Boolean = false
}

case class IndexedLeaf[A](lIndex: Option[Long], rIndex: Option[Long], value: A) extends TreeLike[A] with TreeIndex {
  def children = Nil

  def get = Some(value)

  def :+[B >: A : Ordering](b: Node[B]): Node[B] = throw TreeException("not implemented")

  // treeMaker.tree(b) :+ this
  override def depth: Int = 1

  override def render = s"""$value"""

  override def toString = s"""L("$value")"""

  def isEmpty: Boolean = false
}

object GenericTree {
  def apply[A](as: A*): Node[A] = as.toList match {
    case Nil => Empty.asInstanceOf[Node[A]]
    case h :: t =>
      apply(h, List(apply(t: _*)))
  }

  def apply[A](as: List[A]): Node[A] = apply(as: _*)
}

object BinaryTree {
  def apply[A: Ordering](as: A*): TreeLike[A] = as.toList match {
    case Nil => Empty.asInstanceOf[TreeLike[A]]
    case h :: Nil => BinaryTree(h, Empty, Empty)
    case h :: t => (apply(h) :+ apply(t: _*)).asInstanceOf[BinaryTree[A]]
  }

  implicit val treeMaker = new TreeMaker {
    def tree[T](node: Node[T]): TreeLike[T] = node match {
      case t: BinaryTree[T] => t
      case Leaf(x) => BinaryTree(x, Empty, Empty)
      case _ => throw TreeException(s"cannot build BinaryTree from $node")
    }
  }
}

case class TreeException(msg: String) extends Exception(msg)

object Tree {

  def join(xo: Option[String], yo: Option[String]): Option[String] = {
    val mt = Some("")
    for (x <- xo.orElse(mt); y <- yo.orElse(mt)) yield x + y
  }

  /**
    * XXX: this method is NOT tail-recursive
    *
    * @param node  the node representing the tree to be indexed
    * @param index the starting value of index
    * @tparam A the underlying type
    * @return an IndexedNode[A]
    *
    *         TODO figure out why we can't actually use IndexedNode as return type
    */
  def createIndexedTree[A](node: Node[A], index: Int): Node[A] with TreeIndex = node match {
    case Leaf(x) => IndexedLeaf[A](Some(index), Some(index + 1), x)
    case BinaryTree(v, l, r) =>
      val rIndex = index + 1 + l.size + r.size
      MutableGenericIndexedTree(Some(index), Some(rIndex), v, Seq(createIndexedTree(l, index), createIndexedTree(r, index + 1 + l.size)))
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

  def find[B >: Nothing](p: Node[B] => Boolean): Option[Node[B]] = None

  def like[B >: Nothing](n: Node[B]): Maybe = ^^
}

case object Open extends Punctuation("{")

case object Close extends Punctuation("}")
