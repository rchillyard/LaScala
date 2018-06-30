package com.phasmid.laScala

import scala.annotation.tailrec


sealed trait BinaryTree[+T] {
  def depthFirst[U](f: T=>U): Seq[U]
}
case class Node[+T](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  // CONSIDER doing an in-order traversal
  def depthFirst[U](f: T => U): Seq[U] = {
    @tailrec def inner(todo: Seq[BinaryTree[T]], done: List[T]): Seq[U] = todo match {
      case Nil => done map f
      case h::t => h match {
        case Node(v,l,r) => inner(r::l::t, v::done)
        case Empty => inner(t, done)
      }
    }
    inner(Seq(this),Nil)
  }
}
case object Empty extends BinaryTree[Nothing] {
  override def toString = "."

  def depthFirst[U](f: Nothing => U): Seq[U] = Nil
}

case class DepthFirstSearch[T](root: BinaryTree[T]) {
    @tailrec private def inner(todo: Seq[BinaryTree[T]], preOrder: List[T], postOrder: List[T]): (Seq[T],Seq[T]) = todo match {
      case Nil => (preOrder, postOrder)
      case h::t => h match {
        case Node(v,l,r) => inner(r::l::t, preOrder:+v, v::postOrder)
        case Empty => inner(t, preOrder, postOrder)
      }
  }
  private lazy val (pre,post) = inner(Seq(root),Nil,Nil)
  def preOrder: Seq[T] = pre
  def postOrder: Seq[T] = post
  def revPostOrder: Seq[T] = post.reverse
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, Empty, Empty)
}