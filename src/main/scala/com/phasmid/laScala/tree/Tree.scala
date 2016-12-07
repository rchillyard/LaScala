package com.phasmid.laScala.tree

import com.phasmid.laScala._
import com.phasmid.laScala.fp.FP._
import com.phasmid.laScala.fp.{HasKey, Spy}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.language.implicitConversions

 /**
  * Trait which models the tree-like aspects of a tree
  *
  * @tparam A the underlying type of the tree/node
  */
sealed trait Tree[+A] extends Node[A] {

   override implicit val logger = Spy.getLogger(getClass)

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
    case Some(_) => 1
    case _ => 0
  }, _ + _, { _ => false })(List(this), 0)

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
    * @param b the value to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def +:[B >: A : TreeBuilder : NodeParent : HasParent](b: B): Tree[B] = :+(b)

  /**
    * Method to add a value to this tree by simply creating a leaf and calling :+(Node[B])
    *
    * @param b the value to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def :+[B >: A : TreeBuilder : NodeParent : HasParent](b: B): Tree[B] = :+(implicitly[TreeBuilder[B]].buildLeaf(b))

  /**
    * Method to add a node to this tree: because the addition of nodes is not order-dependent this method simply invokes :+
    *
    * NOTE the implementation of this method assumes that the order of the operands is not important.
    *
    * @param node the node to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def +:[B >: A : TreeBuilder : NodeParent : HasParent](node: Node[B]): Tree[B] = this :+ node

  /**
    * Method to add a node to this tree
    *
    * @param node        the node to add
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def :+[B >: A : TreeBuilder : NodeParent : HasParent](node: Node[B]): Tree[B] = addNode(node)


  def addNode[B >: A : TreeBuilder : NodeParent : HasParent](node: Node[B], allowRecursion: Boolean = true): Tree[B] = {
    val no = Spy.spy(s"addNode: $node with existing parent: ", findParent(node))
    val (x, y: Tree[B]) = no orElse Some(this) match {
      case Some(n: Tree[B]) => (n, implicitly[TreeBuilder[B]].buildTree(n.get, n.children :+ node))
      case Some(n: Node[B]) => (n, implicitly[TreeBuilder[B]].buildTree(n.get, Seq(node)))
      case _ => throw TreeException(s"cannot find parent for $node or parent is not of type Tree")
    }
    // Replace node x with node y where x's value matches y's value
    // XXX check that the cast following is justified
    replaceNode(x, y)(_.get == _.get).asInstanceOf[Tree[B]]
  }

  /**
    * Find a suitable and existing parent for the given Node. Note that the node has not yet become part of the this Tree so
    * we can't just check if it's one of the children of a candidate parent.
    *
    * NOTE: this method will take time proportional to the number of nodes already in the tree and will visit each node if isParent always returns false
    *
    * @param node the node to be added
    * @tparam B the underlying type of the node
    * @return a (optional) node which would be a suitable parent for node
    */
  def findParent[B >: A : NodeParent : HasParent](node: Node[B]): Option[Node[B]] = find(implicitly[NodeParent[B]].isParent(_,node))

  /**
    * Iterate on the values of this tree
    *
    * @param depthFirst if true then we iterate depth-first, else breadth-first (defaults to true)
    * @return an iterator of values
    */
  def iterator(depthFirst: Boolean = true): Iterator[A] = for (n <- nodeIterator(depthFirst); a <- n.get) yield a

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
      (children zip ns).foldLeft(Kleenean(None))(_ :&& cf(_))
    case _ => Kleenean(false)
  }

  /**
    * Method to determine if this Tree includes the given node
    *
    * @param node the node to be searched for
    * @tparam B the underlying type of n
    * @return true if node n is found in the sub-tree defined by this
    */
  def includes[B >: A](node: Node[B]): Boolean = (this compareValues node)() match {
    case Some(true) => true
    case _ => children.foldLeft[Boolean](false)((b, n) => if (b) b else n includes node)
  }

//    val f: Node[B] => Maybe = { n => n compareValues node }
//    val g: (Boolean, Maybe) => Boolean = { (b, m) => (b |: m) ().getOrElse(false) }
//    Parent.traverse[Node[B], Maybe, Boolean](f, g, { x => x })(List(this), false)


  /**
    * Determine if this subtree includes a value equal to b
    *
    * CONSIDER renaming this back to just includes (as it was previously)
    *
    * @param b the value to be searched
    * @tparam B the type of b
    * @return true if value b is found in the sub-tree defined by this
    */
  def includesValue[B >: A](b: B): Boolean = get match {
    case Some(`b`) => true
    case _ => children.foldLeft[Boolean](false)((x, n) => if (x) x else n.includesValue(b))
  }

//    Parent.traverse[Node[B], Boolean, Boolean]({ n => n.get match {
//    case Some(`b`) => true
//    case _ => false
//  }
//  }, _ | _, { x => x })(List(this), false)

  /**
    * Determine if this tree has a subtree matching b which includes c
    *
    * @param subtree the value of the subtree to be searched
    * @param element the value of the element to be found in the subtree
    * @tparam B the type of subtree and element
    * @return true if value element is found in the sub-tree defined by subtree
    */
  def includes[B >: A](subtree: B, element: B): Boolean = find(subtree) match {
    case Some(s: Node[B]) => s.includesValue(element) //Spy.spy(s"includes: ${s.get.get}:${element}",s.includesValue(element), false)
    case _ => false
  }

  /**
    * @param b value to be found, looking at the value of each node in turn (depth first)
    * @tparam B the type of b
    * @return Some(node) where node is the first node to be found with value b
    */
  def find[B >: A](b: B): Option[Node[B]] = find({ n: Node[B] => Spy.spy(s"find: $b in $n",n.get.contains(b))})

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

   def filter[B >: A](p: Node[B] => Boolean): Iterator[Node[B]] = nodeIterator().filter(p)


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

/**
  * Trait which models the node of a tree
  *
  * @tparam A the underlying type of the tree/node
  */
sealed trait Node[+A] extends Renderable {

  implicit val logger = Spy.getLogger(getClass)

  /**
    * @return the value of this node, if any
    */
  def get: Option[A]

  /**
    * Iterate on the nodes which are the descendants of this node (including this node at start or end of result)
    *
    * @param depthFirst if true then we iterate depth-first, else breadth-first (defaults to true)
    * @return an iterator of nodes
    */
  def nodeIterator(depthFirst: Boolean = true): Iterator[Node[A]]

  /**
    * Method to determine if this Node's value is equal to node n's value WITHOUT any recursion.
    *
    * @param n the node to be compared
    * @tparam B the underlying type of n
    * @return true if this is "like" n
    */
  def compareValues[B >: A](n: Node[B]): Maybe = Kleenean(map2(get, n.get)(_ == _))

  /**
    * Method to determine if this Node is a leaf or a branch node
    * @return true if this Node is a leaf
    */
  def isLeaf: Boolean = this match {
    case AbstractLeaf(_) => true
    case Empty => true
    case Branch(_) => false
    case _ => println(s"isLeaf fell through with $this"); false
  }

  /**
    * Calculate the size of this node's subtree
    *
    * @return the size of the subtree
    */
  def size: Int

  /**
    * Method to determine the depth of the tree headed by this Node.
    * Ideally, we would put this method into Tree, but because of the specific (non-tail-recursive)
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

  /**
    * Determine if this subtree includes a value equal to b
    *
    * @param b the value to be searched
    * @tparam B the type of b
    * @return true if value b is found in the sub-tree defined by this
    */
  def includesValue[B >: A](b: B): Boolean

  /**
    * Method to replace a particular node in this tree with another node, but otherwise return a copy of this tree.
    *
    * NOTE: we could provide that B provides evidence of Ordering[B] because this is required for an abstract binary tree
    *
    * @param x the node to be replaced with y
    * @param y the node to replace x
    * @param f function to determine if two nodes are the same
    * @tparam B the underlying type of the new node (and the resulting tree)
    * @return the resulting tree
    */
  def replaceNode[B >: A : TreeBuilder](x: Node[B], y: Node[B])(f: (Node[B], Node[B]) => Boolean): Node[B] = {
    def replaceChildrenNodes(x: Node[B], y: Node[B])(f: (Node[B], Node[B]) => Boolean)(ns: NodeSeq[B]): NodeSeq[B] =
    // TODO remove the match logic
      for (n <- ns) yield n match {
        case t => t.replaceNode(x, y)(f)
      }

    if (f(this, x))
      Spy.spy(s"replaceNode: matched $x with this so return", y) // this and x are the same so simply return y
    else
      this match {
        case Branch(ns) => implicitly[TreeBuilder[B]].buildTree(get, replaceChildrenNodes(x, y)(f)(ns))
        case _ => this
      }
  }
}

/**
  * Trait which models an index that is useful for building an indexed tree, especially an MPTT-type index.
  */
sealed trait TreeIndex {
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
  * @tparam A the underlying type of this Branch
  */
trait Branch[+A] extends Tree[A] {
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
}

/**
  * Trait which combines the Node and TreeIndex traits
  *
  * @tparam A the underlying type of the tree/node
  */
trait IndexedNode[A] extends Node[A] with TreeIndex

/**
  * Trait which combines the IndexedNode and WithKey traits
  *
  * @tparam A the underlying type of the tree/node
  */
trait IndexedNodeWithKey[A] extends IndexedNode[A] with WithKey

/**
  * This trait forms the basis for a type class which can determine if one node is the parent of another.
  *
  * NOTE: this method is intended for existing trees -- it is not for use while building a tree
  *
  * @tparam A the underlying type of the Nodes
  */
trait NodeParent[A] {
  def isParent(parent: Node[A], child: Node[A]): Boolean
}

/**
  * Trait which defines behavior of something which can be rendered as a String
  */
trait Renderable {
  /**
    * Create a String which represents this Renderable object in a manner that is convenient for showing a whole tree.
    *
    * @return an appropriate String
    */
  def render(indent: Int = 0): String
}

/**
  * Trait to define a method for building a tree from a Node.
  * CONSIDER making this trait covariant in A
  * @tparam A the underlying type of the Node(s) and Tree
  */
trait TreeBuilder[A] {
  /**
    * Build a new tree, given a value and child nodes
    * @param maybeValue the (optional) value which the new tree will have at its root
    * @param children the the children of the node
    * @return a tree the (optional) value at the root and children as the immediate descendants
    */
  def buildTree(maybeValue: Option[A], children: Seq[Node[A]]): Tree[A]

  /**
    * Build a new leaf, given a value
    * @param a the value for the leaf
    * @return a node which is a leaf node
    */
  def buildLeaf(a: A): Node[A]
}

/**
  * This trait is used for a type class that enables a T value to yield a String value that relates to the parent of the node containing the value.
  * This is typically used when building a tree from a list of parent-child relationships.
  * I tried to make the key type polymorphic but I just couldn't do it, even with a type definition in the trait.
  * The problem is subtle and could be revisited at some later date.
  *
  * @tparam T the underlying type
  */
trait HasParent[T] {

  /**
    * Get the key for the parent of the node containing t as its value
    *
    * @param t the value
    * @return the parent key wrapped as an Option
    */
  def getParentKey(t: T): Option[String]

  /**
    * In the event that a parent cannot be found, we sometimes have to create a new parent.
    * @param t the value
    * @return a new node wrapped as an Option
    */
  def createParent(t: T): Option[Node[T]]
}

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
  * Case class for a leaf whose value is a
  *
  * @param a the value of this leaf
  * @tparam A the underlying type of this Leaf
  */
case class Leaf[+A](a: A) extends AbstractLeaf[A](a)

/**
  * Case class for a binary tree where the nodes themselves do not have values
  *
  * @param left the left-side node
  * @param right the right-side node (assumed to come after the left side node)
  * @tparam A the underlying type which must be Orderable.
  */
case class UnvaluedBinaryTree[+A: Ordering](left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](left, right) {

  /**
    * @return None
    */
  def get = None
}

/**
  * Case class for a binary tree with a value at each node
  *
  * @param a the value of the root node of this binary tree
  * @param left the left-side node
  * @param right the right-side node (assumed to come after the left side node)
  * @tparam A the underlying type which must be Orderable.
  */
case class BinaryTree[+A: Ordering](a: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](left, right) {

  /**
    * @return None
    */
  def get = None

}

/**
  * Case class for an indexed leaf
  *
  * @param lIndex the (optional) index to the left of the leaf
  * @param rIndex the (optional) index to the right of the leaf
  * @param value the leaf value
  * @tparam A the underlying type of this Leaf
  */
case class IndexedLeaf[A](lIndex: Option[Long], rIndex: Option[Long], value: A) extends AbstractLeaf[A](value) with TreeIndex {
  override def depth: Int = 1

  override def render(indent: Int): String = value match {
    case renderable: Renderable => renderable.render(indent)
    case _ => s"""${Renderable.prefix(indent)}$value [$lIndex:$rIndex]"""
  }

  override def toString = s"""L("$value")"""
}

/**
  * Case class for an indexed leaf with key
  *
  * @param lIndex the (optional) index to the left of the leaf
  * @param rIndex the (optional) index to the right of the leaf
  * @param value the leaf value
  * @tparam A the underlying type of this Leaf which must implement HasKey
  */
case class IndexedLeafWithKey[A : HasKey](lIndex: Option[Long], rIndex: Option[Long], value: A) extends AbstractLeaf[A](value) with IndexedNodeWithKey[A] {
  override def depth: Int = 1

  override def toString = s"""L("$value")"""

  def key: String = implicitly[HasKey[A]].getKey(value)
}

/**
  * A mutable (temporary) tree used as a preliminary to building an MPTT index
  *
  * @param lIndex the (optional) index to the left of the node
  * @param rIndex the (optional) index to the right of the node
  * @param value the leaf value
  * @param children the children of this node
  * @tparam A the underlying type of this Branch
  */
case class MutableGenericIndexedTree[A](var lIndex: Option[Long], var rIndex: Option[Long], var value: Option[A], var children: Seq[Node[A]]) extends Branch[A] with IndexedNode[A] {
  def get: Option[A] = value
}

/**
  * The exception class for Trees
  * @param msg the message to be yielded by the exception
  * @param cause the cause (defaults to null)
  */
case class TreeException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

/**
  * Empty object which is necessary for (non-ideal) BinaryTrees.
  * There should be no need for a GeneralTree to reference an Empty but it might happen.
  */
case object Empty extends AbstractEmpty

/**
  * Base class for binary trees
  * @param left the left node
  * @param right the right node
  * @tparam A the underlying type of this binary tree
  */
abstract class AbstractBinaryTree[+A: Ordering](left: Node[A], right: Node[A]) extends Branch[A] {
  assume(AbstractBinaryTree.isOrdered(left, right), s"$left is not ordered properly with $right")

  /**
    * Return a sequence made up of left and right
    *
    * @return the immediate descendants (children) of this branch
    */
  def children: Seq[Node[A]] = Seq(left, right)

}

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

  def includesValue[B >: A](b: B): Boolean = a == b

  def render(indent: Int): String = a match {
    case renderable: Renderable => renderable.render(indent)
    case _ => s"${Renderable.prefix(indent)}$a"
  }
}

/**
  * Base class for Empty nodes
  */
abstract class AbstractEmpty extends Tree[Nothing] {
  def nodeIterator(depthFirst: Boolean): Iterator[Node[Nothing]] = Iterator.empty

  def get = None

  /**
    * @return the immediate descendants (children) of this branch
    */
  def children: Seq[Tree[Nothing]] = Seq.empty

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

  def includesValue[B >: Nothing](b: B): Boolean = false

  def size: Int = 0

  def nodeIterator(depthFirst: Boolean): Iterator[Node[Nothing]] = Iterator.empty

  def get: Option[Nothing] = None

  def render(indent: Int): String = x

  def depth: Int = 0
}

object Renderable {
  def prefix(indent: Int): String = "  " *indent
}

object AbstractLeaf {
  def unapply[A](t: Node[A]): Option[A] = t match {
    case Leaf(x) => Some(x)
    case IndexedLeaf(_,_,x) => Some(x)
    case IndexedLeafWithKey(_,_,x) => Some(x)
    case _ => None
  }
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
      case tl: Tree[A] => tl.children
      case _ => Seq.empty // XXX: this should be OK
    }
  }
}

object Tree {
  // TODO we want the key value to implement ordering, not the value itself
  def populateOrderedTree[A: Ordering : TreeBuilder : NodeParent : HasParent](values: Seq[A]): Tree[A] = {
    values match {
      case h :: t =>
        var result: Tree[A] = implicitly[TreeBuilder[A]].buildTree(Some(h), Seq())
        for (w <- t) {
          result = result :+ Leaf(w)
        }
        result
    }
  }

  def populateGeneralTree[A : TreeBuilder : NodeParent : HasParent](values: Seq[A]): Tree[A] = {
    values match {
      case h :: t =>
        var result: Tree[A] = implicitly[TreeBuilder[A]].buildTree(Some(h), Seq())
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
  def createIndexedTree[A : HasKey](node: Node[A], index: Int = 0): IndexedNode[A] = {
    @tailrec
    def inner(r: Seq[Node[A]], work: (Int, Seq[Node[A]])): Seq[Node[A]] = work._2 match {
      case Nil => r
      case h :: t => inner(r :+ createIndexedTree(h, work._1), (work._1 + h.size, t))
    }
    node match {
      case Leaf(x) => IndexedLeafWithKey[A](Some(index), Some(index + 1), x)
      case UnvaluedBinaryTree(l, r) =>
        val rIndex = index + 1 + l.size + r.size
        MutableGenericIndexedTree(Some(index), Some(rIndex), None, inner(Nil, (index, Seq(l,r))))
      case GeneralTree(a, ans) =>
        val rIndex = index + 1 + (ans map (_.size) sum)
        MutableGenericIndexedTree(Some(index), Some(rIndex), Some(a), inner(Nil, (index, ans)))
      case Empty => EmptyWithIndex.asInstanceOf[IndexedNode[A]] // XXX check this is OK
      case _ => throw TreeException(s"can't created IndexedTree from $node")
    }
  }
}

object GeneralTree {
  // CONSIDER moving these traits up
  trait GeneralTreeBuilder[A] extends TreeBuilder[A] {
    def buildTree(maybeValue: Option[A], children: Seq[Node[A]]): Tree[A] = GeneralTree(maybeValue.get,children)
    def buildLeaf(a: A): Node[A] = Leaf(a)
  }
  implicit object GeneralTreeBuilderInt extends GeneralTreeBuilder[Int]
  implicit object GeneralTreeBuilderString extends GeneralTreeBuilder[String]

  // CONSIDER reimplementing this in the more general form of BranchNodeParent
  trait GeneralNodeParent[A] extends NodeParent[A] {
    def isParent(parent: Node[A], child: Node[A]): Boolean = parent match {
      case Branch(ns) => ns.contains(child)
      case _ => false
    }
  }
  implicit object GeneralNodeParentInt extends GeneralNodeParent[Int]
  implicit object GeneralNodeParentString extends GeneralNodeParent[String]

  trait NodeTypeParent[A] extends HasParent[A] {
    def createParent(a: A): Option[Node[A]] = None

    def getParentKey(a: A): Option[String] = None
  }
  implicit object GeneralNodeTypeParentInt extends NodeTypeParent[Int]
  implicit object GeneralNodeTypeParentString extends NodeTypeParent[String]

}

object UnvaluedBinaryTree {
  implicit val logger = Spy.getLogger(getClass)

  abstract class UnvaluedBinaryTreeBuilder[A : Ordering] extends TreeBuilder[A] {
    def buildTree(maybeValue: Option[A], children: Seq[Node[A]]): Tree[A] = {
      val ns = children filterNot (_ == Empty)
      ns.size match {
        case 0 => buildTree(Empty, Empty)
        case 1 => buildTree(ns.head, Empty)
        case 2 => buildTree(ns.head, ns.last)
        case 3 =>
          // If there are more than two children, then the last element needs to be ordered appropriately amongst the first two children
          val l = ns.head
          val r = ns(1)
          val x = ns(2)
          assert(AbstractBinaryTree.isOrdered(l,r), "logic error: l,r are not properly ordered")
          if (AbstractBinaryTree.isOrdered(x, l)) buildTree(None, Seq(x, buildTree(l, r)))
          else buildTree(None, Seq(l, buildTree(r, x)))
        case _ => throw TreeException(s"buildTree with value: $maybeValue and children: $children")
      }
    }

    // TODO simplify this: now that we explicitly order n1, n2 we don't have to be so particular about the various non-overlapping cases
    def buildTree(x: Node[A], y: Node[A]): Tree[A] = {
      // XXX if x,y are ordered correctly (or overlapping) we create n1, n2 in same order, otherwise we flip the order
      val (n1, n2) = if (AbstractBinaryTree.isOrdered(x, y)) (x,y) else (y,x)
      assert(Spy.spy(s"isOrdered n1=$n1, n2=$n2",AbstractBinaryTree.isOrdered(n1,n2)), "logic error")
      n1 match {
        case UnvaluedBinaryTree(l, r) =>
          Spy.spy(s"buildTree from UnvaluedBinaryTree: ($l, $r) and $n2",())
          val pair =
            if (Spy.spy(s"isOverlap1 $n2 $l", AbstractBinaryTree.isOverlap(n2, l)))
              Spy.spy("type1", (buildTree(l, n2), r)) // type 1
            else if (Spy.spy(s"isOverlap2 $n2 $r", AbstractBinaryTree.isOverlap(n2, r)))
              Spy.spy("type2", (l, buildTree(r, n2))) // type 2
            else {
              if (Spy.spy(s"isOrdered3 $l $n2", AbstractBinaryTree.isOrdered(l, n2)))
                if (Spy.spy(s"isOrdered4 $n2 $r", AbstractBinaryTree.isOrdered(n2, r)))
                  Spy.spy("type3", (apply(l, n2), r)) // type 3
                else
                  Spy.spy("type4", (n1, n2)) // type 4
              else
                Spy.spy("type5", (n2, n1)) // type 5
            }
          apply(pair._1, pair._2)
        case p@Leaf(_) =>
          n2 match {
            case UnvaluedBinaryTree(l, r) =>
              Spy.spy(s"buildTree (leaf) from UnvaluedBinaryTree: ($l, $r) and $p", ())
              val pair =
                if (Spy.spy(s"isOverlap1 (leaf) $p $l", AbstractBinaryTree.isOverlap(p, l)))
                  Spy.spy("type1 (leaf)", (buildTree(l, p), r)) // type 1
                else if (Spy.spy(s"isOverlap2 (leaf) $p $r", AbstractBinaryTree.isOverlap(p, r)))
                  Spy.spy("type2 (leaf)", (l, buildTree(r, p))) // type 2
                else {
                  if (Spy.spy(s"isOrdered3 (leaf) $l $p", AbstractBinaryTree.isOrdered(l, p)))
                    if (Spy.spy(s"isOrdered4 (leaf) $p $r", AbstractBinaryTree.isOrdered(p, r)))
                      Spy.spy("type3 (leaf)", (apply(l, p), r)) // type 3
                    else
                      Spy.spy("type4 (leaf)", (n2, p)) // type 4
                  else
                    Spy.spy("type5 (leaf)", (p, n2)) // type 5
                }
              apply(pair._1, pair._2)
            case q@Leaf(_) => if (AbstractBinaryTree.isOrdered(p,q)) apply(p,q) else apply(q,p)
            case _ => throw TreeException(s"treeBuilder not implemented for Leaf $p and $n1")
          }
        case Empty => apply(x, Empty) // TODO check this is OK
        case _ => throw TreeException(s"treeBuilder not implemented for $n1")
      }
    }
      def buildLeaf(a: A): Node[A] = Leaf(a)
    }

  implicit object UnvaluedBinaryTreeBuilderInt extends UnvaluedBinaryTreeBuilder[Int]
  implicit object UnvaluedBinaryTreeBuilderString extends UnvaluedBinaryTreeBuilder[String]

  trait UnvaluedBinaryNodeParent[A] extends NodeParent[A] {
    def isParent(parent: Node[A], child: Node[A]): Boolean = parent match {
      case Branch(ns) => ns.contains(child)
      case _ => false
    }
  }
  implicit object UnvaluedBinaryNodeParentInt extends UnvaluedBinaryNodeParent[Int]
  implicit object UnvaluedBinaryNodeParentString extends UnvaluedBinaryNodeParent[String]

  trait NodeTypeHasParentWithStringKey[A] extends HasParent[A] {
    def createParent(a: A): Option[Node[A]] = None

    def getParentKey(a: A): Option[String] = None
  }

  implicit object UnvaluedBinaryNodeTypeHasParentWithStringKeyInt$ extends NodeTypeHasParentWithStringKey[Int]

  implicit object UnvaluedBinaryNodeTypeHasParentWithStringKeyString$ extends NodeTypeHasParentWithStringKey[String]

}

object BinaryTree {
  // TODO implement this properly, given that there are values in BinaryTree
  // TODO eliminate danger of infinite recursion in this method, perhaps make it tail-recursive
  implicit def treeBuilder[A: Ordering](n1: Node[A], n2: Node[A]): Tree[A] = n1 match {
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
    * Extractor for use in pattern matching
    * @param t the tree
    * @tparam A the tree type
    * @return an optional tuple of two children
    */
  def unapply[A](t: AbstractBinaryTree[A]): Option[(Node[A],Node[A])] = Some((t.children.head,t.children.tail.head))

  /**
    * Method to determine if node a compares as less than node b
    *
    * @param a node a
    * @param b node b
    * @tparam A the underlying node type
    * @return true if node a compares as less than node b
    */
  def isOrdered[A: Ordering](a: Node[A], b: Node[A]): Boolean = isOrdered(compare(a, b)).toBoolean(true)

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
    * @param anL the left-node
    * @param anR the right-node
    * @tparam A the underlying type of the nodes and comparisons
    * @return true if the proposition is proven
    */
  def compare[A: Ordering](anL: Node[A], anR: Node[A]): Seq[Int] = anL match {
    case AbstractLeaf(a) => compare(a, anR)
    case Branch(ans) => anR match {
      case AbstractLeaf(b) => compare(b, ans).map(-_)
      case Branch(bns) => compare(ans, bns)
      case Empty => Seq(0)
    }
    case Empty => Seq(0)
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
    case Empty => Seq(0)
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
  def unapply[A](e: Tree[A]): Option[(NodeSeq[A])] = Some(e.children)
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
