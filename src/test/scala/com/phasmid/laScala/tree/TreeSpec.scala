package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class TreeSpec extends FlatSpec with Matchers {

  behavior of "UnsortedTree"
  it should "apply correctly with varargs" in {
    val tree1 = UnsortedTree(1,Seq(UnsortedTree(2,Seq(UnsortedTree(3,Seq(Empty))))))
    val tree2 = UnsortedTree(1,2,3)
    tree1 shouldBe tree2
  }
  it should "apply correctly with List" in {
    val tree1 = UnsortedTree(1,Seq(UnsortedTree(2,Seq(UnsortedTree(3,Seq(Empty))))))
    val tree2 = UnsortedTree(1,2,3)
    tree1 shouldBe tree2
  }

  behavior of "BinaryTree"
  it should "apply correctly with varargs" in {
    val tree = BinaryTree(1,2,3)
    tree.includes(1) shouldBe true
    tree.depth shouldBe 3
    tree.render shouldBe "1{2{3}}"
  }
  it should "apply correctly with List" in {
    val tree1 = UnsortedTree(1,Seq(UnsortedTree(2,Seq(UnsortedTree(3,Seq(Empty))))))
    val tree2 = UnsortedTree(1,2,3)
    tree1 shouldBe tree2
  }

  behavior of ":+"
  it should "work" in {
    import BinaryTree._
    val x: BinaryTree[String] = BinaryTree("A","C","D").asInstanceOf[BinaryTree[String]]
    val y = x :+ Leaf("B")
    y.size shouldBe 4
    y.depth shouldBe 3
    y.render shouldBe "A{{B}C{D}}"
  }

  behavior of "get"
  it should "work" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1,Seq(Leaf(2),Leaf(3)))
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
    tree.includes(0) shouldBe false
  }

  behavior of "depth"
  it should "work for UnsortedTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1,Seq(Leaf(2),Leaf(3)))
    tree.depth shouldBe 2
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1,2,3)
    tree.depth shouldBe 3
    val tree2 = tree.asInstanceOf[BinaryTree[Int]] :+ BinaryTree(1)
    tree2.asInstanceOf[BinaryTree[Int]].depth shouldBe 3
  }

  behavior of "size"
  it should "work for UnsortedTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1,Seq(Leaf(2),Leaf(3)))
    tree.size shouldBe 3
  }
  it should "work for BinaryTree" in {
    val tree: BinaryTree[Int] = BinaryTree(1,2,3).asInstanceOf[BinaryTree[Int]]
    tree.size shouldBe 3
    val tree2 = tree :+ BinaryTree(1)
    tree2.size shouldBe 4
  }

  behavior of "includes"
  it should "work for UnsortedTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1,Seq(Leaf(2),Leaf(3)))
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1,2,3)
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
  }

  behavior of "real-life BinaryTree"
  it should "build correctly" in {
    def populateTree(values: Seq[String]): TreeLike[String] = {
      import BinaryTree._
      var result = BinaryTree[String]()
      for (w <- values) {
        result = (result :+ Leaf(w)).asInstanceOf[TreeLike[String]]
      }
      result
    }
    val url = getClass.getResource("flatland.txt")
    println(url)
    val uo = Option(url)
    uo should matchPattern { case Some(_) => }
    val so = uo map {_.openStream}
    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
    val z: Seq[String] = wso match {
      case Some(ws) => ws map {_.toLowerCase} filterNot {_.isEmpty} distinct
      case _ => Seq[String]()
    }
//    println(z.take(40))
    val tree = populateTree(z)
    tree.includes("flatland") shouldBe true
    tree.depth shouldBe 14
    tree.size shouldBe 177
//    println(tree.render)
  }

}
