package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
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
    val indexedTree = Tree.createIndexedTree(tree.asInstanceOf[BinaryTree[Int]])
    println(s"indexedTree: $indexedTree")
  }
  it should "support MPTT correctly" in {
    val tree = BinaryTree(1,2,3)
    val indexedTree = Tree.createIndexedTree(tree.asInstanceOf[BinaryTree[Int]])
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

  behavior of "compareValues"
  it should "work for Leaf" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree1: Node[Int] = Leaf(1)
    val tree2: Node[Int] = Leaf(1)
    tree2.compareValues(tree1) should matchPattern {case Kleenean(Some(true)) => }
  }
  behavior of "like"
  it should "work for Leaf" in {
    implicit val builder = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = GenericTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree1: Node[Int] = Leaf(1)
    val tree2: Node[Int] = Leaf(1)
    tree2.like(tree1) should matchPattern {case Kleenean(Some(true)) => }
  }
  it should "work for BinaryTree" in {
    val tree1: Node[Int] = BinaryTree(1,2,3)
    val tree2: Node[Int] = BinaryTree(1,2,3)
    tree2.like(tree1) should matchPattern {case Kleenean(Some(true)) => }
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
    tree.find(2) should matchPattern { case Some(Leaf(2)) => }
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1,2,3)
    tree.find(1) should matchPattern { case Some(BinaryTree(_,_,_)) => }
    tree.find(4) should matchPattern { case None => }
  }

  behavior of "real-life BinaryTree"
  it should "build correctly unsorted" in {
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
    val nodes = tree.iterator
    val words = for (n <- nodes; v <- n.get) yield v.toString
    println(words.toSeq mkString " ")
    tree.render shouldBe flatLandTree
    val summaries = for (x <- tree.iterator) yield x.summary
    summaries foreach println
  }
  it should "build correctly sorted" in {
    val uo = Option(getClass.getResource("flatland.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map {_.openStream}
    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
    import scala.language.postfixOps
    val z: Seq[String] = wso match {
      case Some(ws) => (ws map {_.toLowerCase} filterNot {_.isEmpty} distinct) sorted
      case _ => Seq[String]()
    }
    val tree = TreeLike.populateTree(z)
    tree.includes("flatland") shouldBe true
    tree.depth shouldBe 90
    tree.size shouldBe 177
    tree.render shouldBe flatLandTree
  }

  behavior of "createIndexedTree"
  it should "work for BinaryTree" in {
    val tree = BinaryTree("A", "B", "C").asInstanceOf[BinaryTree[String]] :+ BinaryTree("Catfish")
    tree.includes("Catfish") shouldBe true
    val indexedTree = Tree.createIndexedTree(tree)
  }
  it should "work with find" in {
    val tree = BinaryTree("A", "B", "C").asInstanceOf[BinaryTree[String]] :+ BinaryTree("Catfish")
    val indexedTree = Tree.createIndexedTree(tree)
    val nco = indexedTree.find("C")
    nco should matchPattern { case Some(MutableGenericIndexedTree(_,_,_,_)) => }
    val nxo = indexedTree.find("Catfish")
    nxo should matchPattern { case Some(MutableGenericIndexedTree(_,_,"Catfish",_)) => }
    val ndo = indexedTree.find("D")
    ndo should matchPattern { case None => }
    indexedTree.includes(nco.get) shouldBe true
    indexedTree.includes(nxo.get) shouldBe true
  }

  val flatLandTree = "a{{about}above{{actually}ago{{alas}all{{and}another{{anything}appear{{are}as{{at}back{{be}because{{become}becoming{{been}below{{bringing}but{{by}call{{can}ceased{{circle}clearer{{condition}contrary{{correct}could{{country}countrymen{{dare}demonstrate{{described}distinguish{{down}drawing{{edge}edges{{exactly}except{{eye}far{{few}figure{{figures}find{{fixed}flatland{{flatlander}freely{{from}gradually{{happy}hard{{has}have{{hexagons}higher{{i}imagine{{impossible}in{{inhabitants}instead{{into}is{{it}its{{kind}last{{leaning}least{{like}line{{lines}live{{look}lower{{luminous}make{{middle}mind{{more}move{{moving}much{{my}nature{{necessity}nor{{not}nothing{{notion}now{{of}on{{once}one{{only}opened{{or}other{{our}oval{{over}p{{paper}penny{{pentagons}perceive{{place}placed{{places}power{{pretty}privileged{{readers}remaining{{rising}said{{say}see{{shadows}sheet{{should}sight{{sinking}so{{solid}space{{speedily}squares{{straight}such{{suppose}surface{{table}tables{{that}the{{their}them{{then}there{{things}this{{thus}to{{triangles}universe{{upon}us{{vast}very{{view}views{{visible}was{{we}were{{what}when{{which}who{{will}with{{without}world{{years}you{{your}yourself}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
}
