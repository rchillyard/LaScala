package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class TreeSpec extends FlatSpec with Matchers {

  behavior of "GenericTree"
  it should "apply correctly with varargs" in {
    val tree1 = GenericTree(1,Seq(GenericTree(2,Seq(GenericTree(3,Seq(Empty))))))
    val tree2 = GenericTree(1,2,3)
    tree1 shouldBe tree2
  }
  it should "apply correctly with List" in {
    val tree1 = GenericTree(1,Seq(GenericTree(2,Seq(GenericTree(3,Seq(Empty))))))
    val tree2 = GenericTree(1,2,3)
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
    val tree1 = GenericTree(1,Seq(GenericTree(2,Seq(GenericTree(3,Seq(Empty))))))
    val tree2 = GenericTree(1,2,3)
    tree1 shouldBe tree2
  }
  it should "support createIndexedTree correctly" in {
    val tree = BinaryTree(1,2,3)
    val indexedTree = Tree.createIndexedTree(tree.asInstanceOf[BinaryTree[Int]], 0)
    println(s"indexedTree: $indexedTree")
  }
  it should "support MPTT correctly" in {
    val tree = BinaryTree(1,2,3)
    val indexedTree = Tree.createIndexedTree(tree.asInstanceOf[BinaryTree[Int]], 0)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[Int]])
    println(mptt)
  }

  behavior of ":+"
  it should "work" in {
    import BinaryTree._
    val x: BinaryTree[String] = BinaryTree("A","C","D").asInstanceOf[BinaryTree[String]]
    println(x)
    val y: BinaryTree[String] = (x :+ Leaf("B")).asInstanceOf[BinaryTree[String]]
    println(y)
    y.size shouldBe 4
    y.depth shouldBe 3
    y.render shouldBe "A{{B}C{D}}"
    val z = (y :+ Leaf("Alpha")).asInstanceOf[BinaryTree[String]] :+ Leaf("Bravo")
    println(z)
  }

  behavior of "get"
  it should "work" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = GenericTree(1,Seq(Leaf(2),Leaf(3)))
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
    tree.includes(0) shouldBe false
  }

  behavior of "depth"
  it should "work for GenericTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = GenericTree(1,Seq(Leaf(2),Leaf(3)))
    tree.depth shouldBe 2
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1,2,3)
    tree.depth shouldBe 3
    val tree2 = tree.asInstanceOf[BinaryTree[Int]] :+ BinaryTree(1)
    tree2.asInstanceOf[BinaryTree[Int]].depth shouldBe 3
  }

  behavior of "size"
  it should "work for GenericTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = GenericTree(1,Seq(Leaf(2),Leaf(3)))
    tree.size shouldBe 3
  }
  it should "work for BinaryTree" in {
    val tree: BinaryTree[Int] = BinaryTree(1,2,3).asInstanceOf[BinaryTree[Int]]
    tree.size shouldBe 3
    val tree2 = tree :+ BinaryTree(1)
    tree2.size shouldBe 4
  }

  behavior of "includes"
  it should "work for GenericTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = GenericTree(1,Seq(Leaf(2),Leaf(3)))
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

  behavior of "find"
  it should "work for GenericTree" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = GenericTree(1,Seq(Leaf(2),Leaf(3)))
    tree.find(_.get.contains(2)) should matchPattern { case Some(Leaf(2)) => }
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1,2,3)
    tree.find(_.get.contains(1)) should matchPattern { case Some(BinaryTree(_,_,_)) => }
    tree.find(_.get.contains(4)) should matchPattern { case None => }
  }

  behavior of "real-life BinaryTree"
  it should "build correctly" in {
    val uo = Option(getClass.getResource("flatland.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map {_.openStream}
    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
    import scala.language.postfixOps
    val z: Seq[String] = wso match {
      case Some(ws) => ws map {_.toLowerCase} filterNot {_.isEmpty} distinct
      case _ => Seq[String]()
    }
    val tree = TreeLike.populateTree(z)
    tree.includes("flatland") shouldBe true
    tree.depth shouldBe 15
    tree.size shouldBe 177
  }

  behavior of "createIndexedTree"
  it should "work for BinaryTree" in {
    val tree = BinaryTree("A", "B", "C").asInstanceOf[BinaryTree[String]] :+ BinaryTree("Catfish")
    tree.includes("Catfish") shouldBe true
    val indexedTree = Tree.createIndexedTree(tree,0)
  }
  it should "work with find" in {
    val tree = BinaryTree("A", "B", "C").asInstanceOf[BinaryTree[String]] :+ BinaryTree("Catfish")
    tree.includes("Catfish") shouldBe true
    val indexedTree = Tree.createIndexedTree(tree,0)
    val nodeC = indexedTree.find(_.get.contains("C"))
    nodeC should matchPattern { case Some(MutableGenericIndexedTree(_,_,_,_)) => }
    val nodeCatfish = indexedTree.find(_.get.contains("Catfish"))
    nodeCatfish should matchPattern { case Some(MutableGenericIndexedTree(_,_,"Catfish",_)) => }
    val nodeD = indexedTree.find(_.get.contains("D"))
    nodeD should matchPattern { case None => }
    println(s"indexedTree: $indexedTree")
    println(s"nodeC: $nodeC")
    // FIXME
    indexedTree.includes(nodeC) shouldBe true
    println(s"nodeCatfish: $nodeCatfish")
    indexedTree.includes(nodeCatfish) shouldBe true
    println(s"nodeD: $nodeD")
    indexedTree.includes(nodeD) shouldBe false
  }


}
