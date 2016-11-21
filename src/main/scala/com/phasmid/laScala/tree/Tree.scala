package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala._

import scala.language.implicitConversions

/**
  * Trait which models the node of a tree
  *
  * @tparam A the underlying type of the tree/node
  */
sealed trait Node[+A] extends Renderable {

  /**
    * @return the value of this node, if any
    */
  def get: Option[A]

  /**
    * Iterate on the nodes which are the descendants of this node (including this node at start or end of result)
    *
    * @param depthFirst if true then we iterate depth-first, else breadth-first
    * @return an iterator of nodes
    */
  def nodeIterator(depthFirst: Boolean): Iterator[Node[A]]

  /**
    * Method to determine if this Node's value is equal to node n's value WITHOUT any recursion.
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  def compareValues[B >: A](n: Node[B]): Maybe = Kleenean(map2(get, n.get)(_ == _))

  /**
    * Calculate the size of this node's subtree
    *
    * @return the size of the subtree
    */
  def size: Int

  /**
    * Method to determine the depth of the tree headed by this Node.
    * Ideally, we would put this method into TreeLike, but because of the specific (non-tail-recursive)
    * implementation, it is actually more logical to declare it here.
    *
    * @return the maximum depth of the tree headed by this Node
    */
  def depth: Int

  /**
    * Method to determine if this Node includes the given node in its subtree (if any)
    *
    * @param node the node to be searched for
    * @tparam B the underlying type of n
    * @return true if node n is found in the sub-tree defined by this
    */
  def includes[B >: A](node: Node[B]): Boolean
}

/**
  * Trait which models the tree-like aspects of a tree
  *
  * CONSIDER renaming as Tree
  *
  * @tparam A the underlying type of the tree/node
  */
trait TreeLike[+A] extends Node[A] {

  /**
    * @return the immediate descendants (children) of this branch
    */
  def children: Seq[Node[A]]

  /**
    * Calculate the size of this node's subtree
    *
    * @return the size of the subtree
    */
  def size: Int = Parent.traverse[Node[A], Int, Int](_.get match {
    case Some(a) => 1
    case _ => 0
  }, _ + _, { x => false })(List(this), 0)

  /**
    * NOTE: that this is NOT tail-recursive
    *
    * NOTE: I don't think we can use Parent.traverse for depth. Could be wrong.
    *
    * @return the depth of the subtree
    */
  def depth: Int = 1 + children.map(_.depth).max

  /**
    * Method to add a value to this tree: because the addition of values is not order-dependent this method simply invokes :+
    *
    * NOTE the implementation of this method assumes that the order of the operands is not important.
    *
    * @param value       the value to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def +:[B >: A : TreeBuilder : LeafBuilder](value: B): TreeLike[B] = :+(value)

  /**
    * Method to add a value to this tree by simply creating a leaf and calling :+(Node[B])
    *
    * @param value       the value to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def :+[B >: A : TreeBuilder : LeafBuilder](value: B): TreeLike[B] = :+(implicitly[LeafBuilder[B]].buildLeaf(value))

  /**
    * Method to add a node to this tree: because the addition of nodes is not order-dependent this method simply invokes :+
    *
    * NOTE the implementation of this method assumes that the order of the operands is not important.
    *
    * @param node        the node to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def +:[B >: A : TreeBuilder : LeafBuilder](node: Node[B]): TreeLike[B] = this :+ node

  /**
    * Method to add a node to this tree
    *
    * @param node        the node to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def :+[B >: A : TreeBuilder : LeafBuilder](node: Node[B]): TreeLike[B] = implicitly[TreeBuilder[B]].buildTree(this, node)
  /**
    * Iterate on the values of this tree
    *
    * @param depthFirst if true then we iterate depth-first, else breadth-first
    * @return an iterator of values
    */
  def iterator(depthFirst: Boolean): Iterator[A] = for (n <- nodeIterator(depthFirst); a <- n.get) yield a

  /**
    * Create a String which represents the value of this Node and the values of its immediate descendants
    *
    * @return an appropriate String
    */
  def summary: String = {
    val r = new StringBuilder(s""""$get: """") // TODO need to unpack the option
    val l = for (c <- children; x <- c.get.orElse(Some(Empty))) yield x.toString
    r.append(l mkString ", ")
    r.toString
  }

  /**
    * Method to determine if this subtree is "like" the given node (without recursion)
    *
    * @param node the node to be searched for
    * @tparam B the underlying type of n
    * @return true if node n is found in the sub-tree defined by this
    */
  def like[B >: A](node: Node[B]): Maybe = node match {
    case Branch(ns) =>
      def cf(t: (Node[B], Node[B])): Maybe = t._1.compareValues(t._2)
      (children zip ns).foldLeft(Kleenean(None))(_ :& cf(_))
    case _ => Kleenean(false)
  }

  /**
    * Method to determine if this TreeLike includes the given node
    *
    * @param node the node to be searched for
    * @tparam B the underlying type of n
    * @return true if node n is found in the sub-tree defined by this
    */
  def includes[B >: A](node: Node[B]): Boolean = {
    val f: Node[B] => Maybe = { n => n compareValues node }
    val g: (Boolean, Maybe) => Boolean = { (b, m) => (b |: m) ().getOrElse(false) }
    Parent.traverse[Node[B], Maybe, Boolean](f, g, { x => x })(List(this), false)
  }

  /**
    * Determine if this subtree includes a value equal to b
    *
    * @param b the value to be searched
    * @tparam B the type of b
    * @return true if value b is found in the sub-tree defined by this
    */
  def includes[B >: A](b: B): Boolean = Parent.traverse[Node[B], Boolean, Boolean]({ n => n.get match {
    case Some(`b`) => true
    case _ => false
  }
  }, _ | _, { x => x })(List(this), false)

  /**
    * @param b value to be found, looking at the value of each node in turn (depth first)
    * @tparam B the type of b
    * @return Some(node) where node is the first node to be found with value b
    */
  def find[B >: A](b: B): Option[Node[B]] = find({ n: Node[B] => n.get.contains(b)})

  /**
    *
    * @param p the predicate to be applied to the value of each node in turn (depth first)
    * @tparam B the type of b
    * @return Some(node) where node is the first node to be found that satisfies the predicate p
    */
  def find[B >: A](p: Node[B] => Boolean): Option[Node[B]] = {
    def f(n: Node[B]): Option[Node[B]] = toOption(p)(n)
    def g(bno: Option[Node[B]], t: Option[Node[B]]): Option[Node[B]] = bno orElse t
    def q(bno: Option[Node[B]]): Boolean = bno.isDefined
    Parent.traverse[Node[B], Option[Node[B]], Option[Node[B]]](f, g, q)(List(this), None)
  }

  def filter[B >: A](p: Node[B] => Boolean): Iterator[Node[B]] = nodeIterator(true).filter(p)

  //  /**
  //    * Method to safely cast Node n to a TreeLike object.
  //    * Note: This is not an instance method in that it does not reference this
  //    *
  //    * @param n         the node to cast
  //    * @param treeBuilder implicit implementation of TreeMaker trait
  //    * @tparam B the underlying type of node n
  //    * @return if n is already a TreeLike object then return it, otherwise return a sub-tree based on n
  //    */
  //  def asTree[B >: A : Ordering](n: Node[B])(implicit treeBuilder: TreeBuilder[B]): TreeLike[B] = n match {
  //    case t: TreeLike[B] => t
  //    case _ => treeBuilder(n,Empty)
  //  }

//  /**
//    * This was designed for BinaryTree or GeneralTree. Needs testing in general.
//    * It doesn't work properly at present.
//    * Alternatively, can invoke nodeIterator and render each node.
//    * @param z a function to combine a Node with a sequence of Nodes
//    * @tparam B the underlying type of the Nodes
//    * @return a String
//    */
//  def renderRecursive[B >: A](z: (Seq[Node[B]], Node[B]) => Seq[Node[B]]): String =
//    Recursion.recurse[Node[B], String, String]({
//      case Empty => ""
//      case Leaf(x) => x.toString
//      case c@Open => c.render
//      case c@Close => c.render
//      case n => ""
//    }, _ + _, z)(Seq(this), "")
//}

}

trait Renderable {
  /**
    * Create a String which represents this Renderable object in a manner that is convenient for showing a whole tree.
    *
    * @return an appropriate String
    */
  def render(indent: Int = 0): String
}

trait TreeBuilder[A] {
  def buildTree(n1: Node[A], n2: Node[A]): TreeLike[A]
}

trait LeafBuilder[A] {
  def buildLeaf(a: A): Node[A]
}


/**
  * Trait which models an index that is useful for building an indexed tree, especially an MPTT-type index.
  * CONSIDER Restore sealed?
  */
trait TreeIndex {
  /**
    * the left index (the number of other nodes to the "left" of this one
    *
    * @return
    */
  def lIndex: Option[Long]

  /**
    * the right index (the number of other nodes to the "right" of this one
    *
    * @return
    */
  def rIndex: Option[Long]
}

/**
  * A branch of a tree
  *
  * CONSIDER restoring sealed
  *
  * @tparam A the underlying type of this Branch
  */
trait Branch[+A] extends TreeLike[A] {

  /**
    * Iterate on the nodes of this branch
    *
    * TODO: this method is not currently implemented in a tail-recursive manner.
    *
    * @param depthFirst if true then we iterate depth-first, else breadth-first
    * @return an iterator of nodes
    */
  def nodeIterator(depthFirst: Boolean): Iterator[Node[A]] = {
    val z = for (n1 <- children; n2 <- n1.nodeIterator(depthFirst)) yield n2
    val r = if (depthFirst) z :+ this
    else this +: z
    r.toIterator
  }

  /**
    * Create a String which represents this Node and its subtree
    *
    * XXX not tail-recursive.
    * TODO make this tail-recursive, preferably by using traverse... (see renderRecursively in comments)
    * @return an appropriate String
    */
  def render(indent: Int): String = {
    val result = new StringBuilder
    val nodeVal = get match {
      case Some(a: Renderable) => a.render(indent)
      case Some(a) => s"${Renderable.prefix(indent)}$a"
      case _ => ""
    }
    result.append(s"$nodeVal\n")
    val expansion = for (x <- children) yield x.render(indent+1)
    result.append(expansion mkString "\n")
    result.toString
  }
//  iterator(true).mkString(", ")

//  renderRecursive[A]((ns, n) => n
//  match {
//    case Branch(x) => x ++ ns
//    case _ => ns
//  })

}

trait IndexedNode[A] extends Node[A] with TreeIndex

/**
  * A general branch of a tree, where there is a value at the node itself and the number of children is unbounded
  *
  * @param value    the value of the branch
  * @param children the children of this Node
  * @tparam A the underlying type of this Branch
  */
case class GeneralTree[+A](value: A, children: Seq[Node[A]]) extends Branch[A] {
  /**
    * @return Some(value)
    */
  def get = Some(value)
}

/**
  * A leaf whose value is a
  *
  * @param value the value of this leaf
  * @tparam A the underlying type of this Leaf
  */
case class Leaf[+A](value: A) extends AbstractLeaf[A](value) {
  /**
    * Create a String which represents this Node and its subtree (if any)
    *
    * @return an appropriate String
    */
  def render(indent: Int): String = if (value.isInstanceOf[Renderable]) value.asInstanceOf[Renderable].render(indent)
  else s"${Renderable.prefix(indent)}$value"
}

case class UnvaluedBinaryTree[+A: Ordering](left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](left, right) {
  assume(AbstractBinaryTree.isOrdered(left, right), s"$left is not ordered properly with $right")

  /**
    * @return None
    */
  def get = None

}

case class BinaryTree[+A: Ordering](value: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](left, right) {
  assume(AbstractBinaryTree.isOrdered(left, right), s"$left is not ordered properly with $right")

  /**
    * @return None
    */
  def get = None

}

abstract class AbstractBinaryTree[+A: Ordering](left: Node[A], right: Node[A]) extends Branch[A] {

  /**
    * Return a sequence made up of left and right
    *
    * @return the immediate descendants (children) of this branch
    */
  def children: Seq[Node[A]] = Seq(left, right)

}

/**
  * Empty object which is necessary for (non-ideal) BinaryTrees.
  * There should be no need for a GeneralTree to reference an Empty but it might happen.
  */
case object Empty extends AbstractEmpty

case class IndexedLeaf[A](lIndex: Option[Long], rIndex: Option[Long], value: A) extends AbstractLeaf[A](value) with TreeIndex {
  override def depth: Int = 1

  def render(indent: Int) = if (value.isInstanceOf[Renderable]) value.asInstanceOf[Renderable].render(indent)
  else s"""${Renderable.prefix((indent))}$value [$lIndex:$rIndex]"""

  override def toString = s"""L("$value")"""
}

case class MutableGenericIndexedTree[A](var lIndex: Option[Long], var rIndex: Option[Long], var value: Option[A], var children: Seq[Node[A]]) extends Branch[A] with IndexedNode[A] {
  def get = value
}

case class TreeException(msg: String) extends Exception(msg)

/**
  * Abstract base class for a leaf whose value is a
  *
  * @param a the value of this leaf
  * @tparam A the underlying type of this Leaf
  */
abstract class AbstractLeaf[+A](a: A) extends Node[A] {
  def nodeIterator(depthFirst: Boolean): Iterator[Node[A]] = Iterator.single(this)

  def get = Some(a)

  def size: Int = 1

  def depth: Int = 1

  def includes[B >: A](node: Node[B]): Boolean = this == node

  def includes[B >: A](b: B): Boolean = a == b
}

abstract class AbstractEmpty extends TreeLike[Nothing] {
  def nodeIterator(depthFirst: Boolean): Iterator[Node[Nothing]] = Iterator.empty

  def get = None

  /**
    * @return the immediate descendants (children) of this branch
    */
  def children: Seq[TreeLike[Nothing]] = Seq.empty

  /**
    * Create a String which represents this Node and its subtree
    *
    * @return an appropriate String
    */
  def render(indent: Int) = s"${Renderable.prefix(indent)}ø"

}

/**
  * CONSIDER extending AbstractEmpty
  *
  * @param x the String that this punctuation will render as
  */
abstract class Punctuation(x: String) extends Node[Nothing] {
  def includes[B >: Nothing](node: Node[B]): Boolean = false

  def includes[B >: Nothing](b: B): Boolean = false

  def size: Int = 0

  def nodeIterator(depthFirst: Boolean): Iterator[Node[Nothing]] = Iterator.empty

  def get: Option[Nothing] = None

  def render(indent: Int): String = x

  def depth: Int = 0
}

object Renderable {
  def prefix(indent: Int) = "  "*indent
}

object Leaf

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
    def children(t: Node[A]): Seq[Node[A]] = t match {
      case tl: TreeLike[A] => tl.children
      case _ => Seq.empty // XXX: this should be OK
    }
  }
}

object TreeLike {
  // TODO we want the key value to implement ordering, not the value itself
  def populateOrderedTree[A: Ordering : TreeBuilder : LeafBuilder](values: Seq[A]): TreeLike[A] = {
    values match {
      case h :: t =>
        var result: TreeLike[A] = implicitly[TreeBuilder[A]].buildTree(implicitly[LeafBuilder[A]].buildLeaf(h), Empty)
        for (w <- t) {
          result = result :+ Leaf(w)
        }
        result
    }
  }

  def populateGeneralTree[A : TreeBuilder : LeafBuilder](values: Seq[A]): TreeLike[A] = {
    values match {
      case h :: t =>
        var result: TreeLike[A] = implicitly[TreeBuilder[A]].buildTree(implicitly[LeafBuilder[A]].buildLeaf(h), Empty)
        for (w <- t) {
          result = result :+ Leaf(w)
        }
        result
    }
  }

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
  def createIndexedTree[A](node: Node[A], index: Int = 0): Node[A] with TreeIndex = node match {
    case Leaf(x) => IndexedLeaf[A](Some(index), Some(index + 1), x)
    case UnvaluedBinaryTree(l, r) =>
      val rIndex = index + 1 + l.size + r.size
      MutableGenericIndexedTree(Some(index), Some(rIndex), None, Seq(createIndexedTree(l, index), createIndexedTree(r, index + 1 + l.size)))
    case Empty => EmptyWithIndex
    case _ => throw TreeException(s"can't created IndexedTree from $node")
  }
}

object GeneralTree {
  trait GeneralTreeBuilder[A] extends TreeBuilder[A] {
    def buildTree(n1: Node[A], n2: Node[A]): TreeLike[A] = n1 match {
      case GeneralTree(v, ns) => GeneralTree(v, ns :+ n2)
      case Leaf(a) => GeneralTree(a, Seq(n2))
      case _ => throw TreeException(s"not implemented: $n1")
    }
  }
  implicit object GeneralTreeBuilderInt extends GeneralTreeBuilder[Int]

  trait GeneralLeafBuilder[A] extends LeafBuilder[A] {
    def buildLeaf(a: A): Node[A] = Leaf(a)
  }
  implicit object GeneralLeafBuilderInt extends GeneralLeafBuilder[Int]
}

object UnvaluedBinaryTree {
  abstract class UnvaluedBinaryTreeBuilder[A : Ordering] extends TreeBuilder[A] {
    def buildTree(n1: Node[A], n2: Node[A]): TreeLike[A] = n1 match {
      case UnvaluedBinaryTree(l, r) =>
        val pair =
          if (AbstractBinaryTree.isOverlap(n2, l))
            (buildTree(l, n2), r) // type 1
          else if (AbstractBinaryTree.isOverlap(n2, r))
            (l, buildTree(r, n2)) // type 2
          else {
            if (AbstractBinaryTree.isOrdered(l, n2))
              if (AbstractBinaryTree.isOrdered(n2, r))
                (UnvaluedBinaryTree(l, n2), r) // type 3
              else
                (n1, n2) // type 4
            else
              (n2, n1) // type 5
          }
        apply(pair._1, pair._2)
      case l @ Leaf(_) => buildTree(UnvaluedBinaryTree(l, Empty), n2)
      case Empty => buildTree(n2, Empty) // TODO check this is OK
      case _ => throw TreeException(s"treeBuilder not implemented for $n1")
    }
  }
  implicit object UnvaluedBinaryTreeBuilderInt extends UnvaluedBinaryTreeBuilder[Int]
  implicit object UnvaluedBinaryTreeBuilderString extends UnvaluedBinaryTreeBuilder[String]

  trait UnvaluedBinaryLeafBuilder[A] extends LeafBuilder[A] {
    def buildLeaf(a: A): Node[A] = Leaf(a)
  }
  implicit object UnvaluedBinaryLeafBuilderInt extends UnvaluedBinaryLeafBuilder[Int]
  implicit object UnvaluedBinaryLeafBuilderString extends UnvaluedBinaryLeafBuilder[String]
}

object BinaryTree {
  // TODO implement this properly, given that there are values in BinaryTree
  // TODO eliminate danger of infinite recursion in this method, perhaps make it tail-recursive
  implicit def treeBuilder[A: Ordering](n1: Node[A], n2: Node[A]): TreeLike[A] = n1 match {
    case BinaryTree(v, l, r) =>
      val pair =
        if (AbstractBinaryTree.isOverlap(n2, l))
          (treeBuilder(l, n2), r) // type 1
        else if (AbstractBinaryTree.isOverlap(n2, r))
          (l, treeBuilder(r, n2)) // type 2
        else {
          if (AbstractBinaryTree.isOrdered(l, n2))
            if (AbstractBinaryTree.isOrdered(n2, r))
              (BinaryTree(v, l, n2), r) // type 3
            else
              (n1, n2) // type 4
          else
            (n2, n1) // type 5
        }
      apply(v, pair._1, pair._2)
    case Leaf(a) => treeBuilder(BinaryTree(a, Empty, Empty), n2)
    case Empty => treeBuilder(n2, Empty) // TODO check this is OK
    case _ => throw TreeException(s"treeBuilder not implemented for $n1")
  }

  implicit def leafBuilder[A](a: A): Node[A] = Leaf[A](a)
}

object AbstractBinaryTree {
  /**
    * Method to determine if node a compares as less than node b
    *
    * @param a node a
    * @param b node b
    * @tparam A the underlying node type
    * @return true if node a compares as less than node b
    */
  def isOrdered[A: Ordering](a: Node[A], b: Node[A]): Boolean = isOrdered(compare(a, b)).toBoolean(false)

  /**
    * Given a sequence of comparison values, determine if the result is true, false or maybe.
    *
    * @param is the comparison values as a sequence of Ints (either -1, 0, or 1)
    * @return Some(true) if all the values are -1, Some(false) if all the values are +1, otherwise None.
    */
  def isOrdered(is: Seq[Int]): Maybe = {
    val distinct = is.distinct
    if (distinct.size == 1)
      Kleenean(distinct.head).deny
    else
      Kleenean(None)
  }

  /**
    * method to determine if there is an overlap between the values of tree n1 and tree n2
    *
    * @param n1 the first tree (order is not important)
    * @param n2 the second tree
    * @tparam A the underlying tree type
    * @return true if there is an overlap of values, that's to say, the values cannot be completely separated.
    */
  def isOverlap[A: Ordering](n1: Node[A], n2: Node[A]): Boolean = isOrdered(compare(n1, n2)) match {
    case Kleenean(None) => true
    case _ => false
  }

  /**
    * Determine whether all of the nodes on the left are ordered before all the nodes on the right
    *
    * Note: this is not recursive, as you can see by the lack of a return type
    *
    * @param anL the left-node
    * @param anR the right-node
    * @tparam A the underlying type of the nodes and comparisons
    * @return true if the proposition is proven
    */
  def compare[A: Ordering](anL: Node[A], anR: Node[A]): Seq[Int] = anL match {
    case Leaf(a) => compare(a, anR)
    case Branch(ans) => anR match {
      case Leaf(b) => compare(b, ans).map {
        -_
      }
      case Branch(bns) => compare(ans, bns)
      case Empty => Seq(-1)
    }
    case Empty => Seq(-1)
  }

  /**
    * Determine whether the value on the left is ordered before all the nodes on the right
    *
    * TODO: this is recursive
    *
    * @param ans the left-node-sequence
    * @param bns the right-node-sequence
    * @tparam A the underlying type of the nodes and comparisons
    * @return true if the proposition is proven
    */
  def compare[A: Ordering](ans: NodeSeq[A], bns: NodeSeq[A]): Seq[Int] = for (a1 <- ans; r <- compare(a1, bns)) yield r

  /**
    * Determine whether the value on the left is ordered before all the nodes on the right
    *
    * TODO: this is recursive
    *
    * @param an  the left-node
    * @param bns the right-node-sequence
    * @tparam A the underlying type of the nodes and comparisons
    * @return true if the proposition is proven
    */
  def compare[A: Ordering](an: Node[A], bns: NodeSeq[A]): Seq[Int] = for (b <- bns; r <- compare(an, b)) yield r

  /**
    * Determine whether the value on the left is ordered before all the nodes on the right
    *
    * TODO: this is recursive
    *
    * @param a  the left-value
    * @param an the right-node
    * @tparam A the underlying type of the nodes and comparisons
    * @return true if the proposition is proven
    */
  def compare[A: Ordering](a: A, an: Node[A]): Seq[Int] = an match {
    case Leaf(b) => Seq(compare(a, b))
    case Branch(as) => compare(a, as)
    case Empty => Seq(-1)
    case _ => throw TreeException(s"match error for: $a, $an")
  }

  /**
    * Determine whether the value on the left is ordered before all the nodes on the right
    *
    * @param a  the left-value
    * @param as the right-iterator
    * @tparam A the underlying type of the nodes and comparisons
    * @return true if the proposition is proven
    */
  def compare[A: Ordering](a: A, as: NodeSeq[A]): Seq[Int] = for (a1 <- as; r <- compare(a, a1)) yield r

  /**
    * Determine whether the value on the left is ordered before the value on the right
    *
    * @param a the left-value
    * @param b the right-value
    * @tparam A the underlying type of the nodes and comparisons
    * @return -1 if proposition is true; 0 if they are equal; 1 if the proposition if false
    */
  def compare[A: Ordering](a: A, b: A): Int = math.signum(implicitly[Ordering[A]].compare(a, b))
}

object Branch {
  def unapply[A](e: Branch[A]): Option[(NodeSeq[A])] = Some(e.children)
}

object IndexedNode {
  def unapply[A](e: IndexedNode[A]): Option[(Node[A], Long, Long)] =
    for (x <- Option(e); l <- x.lIndex; r <- x.rIndex) yield (x, l, r)
}

case object EmptyWithIndex extends AbstractEmpty with TreeIndex {
  override def toString = "EmptyWIthIndex"

  def lIndex: Option[Long] = None

  def rIndex: Option[Long] = None
}

case object Open extends Punctuation("{")

case object Close extends Punctuation("}")
