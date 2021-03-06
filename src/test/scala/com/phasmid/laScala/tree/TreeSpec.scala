package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp.Spy
import com.phasmid.laScala.tree.AbstractBinaryTree._
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.Logger

import scala.util.Try

/**
  * Created by scalaprof on 10/19/16.
  */
class TreeSpec extends FlatSpec with Matchers {
  private implicit val logger: Logger = Spy.getLogger(getClass)

  implicit object IntStringValueOps$ extends StringValueOps[Int] {
    def getParentKey(v: Int): Option[String] = Some((v / 10).toString)

    def createValueFromKey(k: String, vo: => Option[Int]): Option[Int] = Try(k.toInt).toOption
  }

  behavior of "render"
  it should "work for leaf" in {
    Leaf(42).render() shouldBe "42"
    Leaf(42).render(1) shouldBe "42"
  }
  it should "work for GeneralTree" in {
    val tree = GeneralTree(0, List(Leaf(1), Leaf(2), Leaf(3)))
    tree.render() shouldBe "0--(\n  1,\n  2,\n  3\n)"
  }
  behavior of "compare"
  it should "work for simple values" in {
    compare(1, 2) shouldBe -1
    compare(2, 1) shouldBe 1
    compare(1, 1) shouldBe 0
  }

  it should "work for value-node" in {
    isOrdered(compare(1, Leaf(2))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(2, Leaf(1))) should matchPattern { case Kleenean(Some(false)) => }
    isOrdered(compare(1, Leaf(1))) should matchPattern { case Kleenean(None) => }
  }

  it should "work for value-sequence" in {
    isOrdered(compare(1, Seq(Leaf(3), Leaf(4)))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(3, Seq(Leaf(1), Leaf(2)))) should matchPattern { case Kleenean(Some(false)) => }
    isOrdered(compare(1, Seq(Leaf(1)))) should matchPattern { case Kleenean(None) => }
    isOrdered(compare(2, Seq(Leaf(1), Leaf(3)))) should matchPattern { case Kleenean(None) => }
  }

  it should "work for nodes" in {
    isOrdered(compare(Leaf(1), Leaf(2))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(Leaf(2), Leaf(1))) should matchPattern { case Kleenean(Some(false)) => }
    isOrdered(compare(Leaf(1), Leaf(1))) should matchPattern { case Kleenean(None) => }
    isOrdered(compare(Leaf(1), Empty)) should matchPattern { case Kleenean(None) => }
    isOrdered(compare(Empty, Leaf(1))) should matchPattern { case Kleenean(None) => }
    isOrdered(compare(Leaf(4), UnvaluedBinaryTree(Leaf(3), Empty))) should matchPattern { case Kleenean(Some(false)) => }
    isOrdered(compare(UnvaluedBinaryTree(Leaf(3), Empty), Leaf(4))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(Leaf(3), UnvaluedBinaryTree(Leaf(4), Empty))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(UnvaluedBinaryTree(Leaf(4), Empty), Leaf(3))) should matchPattern { case Kleenean(Some(false)) => }
  }

  it should "work for node-sequence" in {
    isOrdered(compare(Leaf(1), Seq(Leaf(3), Leaf(4)))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(Leaf(3), Seq(Leaf(1), Leaf(2)))) should matchPattern { case Kleenean(Some(false)) => }
    isOrdered(compare(Leaf(1), Seq(Leaf(1)))) should matchPattern { case Kleenean(None) => }
  }

  it should "work for node sequences" in {
    isOrdered(compare(Seq(Leaf(1), Leaf(2)), Seq(Leaf(3), Leaf(4)))) should matchPattern { case Kleenean(Some(true)) => }
    isOrdered(compare(Seq(Leaf(3), Leaf(4)), Seq(Leaf(1), Leaf(2)))) should matchPattern { case Kleenean(Some(false)) => }
    isOrdered(compare(Seq(Leaf(3)), Seq(Leaf(3)))) should matchPattern { case Kleenean(None) => }
  }

  behavior of "isOrdered"
  it should "work with nodes" in {
    isOrdered(Leaf(1), Leaf(2)) shouldBe true
    isOrdered(Leaf(1), Leaf(1)) shouldBe true
    isOrdered(Leaf(2), Leaf(1)) shouldBe false
  }
  behavior of "nodeIterator"

  it should "work properly for GeneralTree" in {
    val tree = GeneralTree(0, List(Leaf(1), Leaf(2), Leaf(3)))
    val ns = tree.nodeIterator()
    ns.size shouldBe 4
  }
  it should "work properly for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    val ns = tree.nodeIterator()
    ns.size shouldBe 7
  }

  behavior of "iterator"

  it should "work properly for GeneralTree" in {
    val tree = GeneralTree(0, List(Leaf(1), Leaf(2), Leaf(3)))
    val ns = tree.iterator(false).toList
    ns.size shouldBe 4
    ns shouldBe List(0, 1, 2, 3)
  }

  it should "work properly for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    val ns = tree.iterator().toList
    ns.size shouldBe 4
    ns shouldBe List(1, 3, 5, 6)
  }

  behavior of "+(node)"

  it should "work correctly for GeneralTree" in {
    import GeneralTree._
    val tree: Tree[Int] = GeneralTree(0, List(Leaf(1), Leaf(2), Leaf(3))) + Leaf(4)
    tree shouldBe GeneralTree(0, List(Leaf(1), Leaf(2), Leaf(3), Leaf(4)))
    tree.iterator().toSeq shouldBe Seq(1, 2, 3, 4, 0)
    tree.iterator(false).toSeq shouldBe Seq(0, 1, 2, 3, 4)
  }
  it should "work correctly for UnvaluedBinaryTree type 1/3" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6))) + Leaf(2)
    tree.iterator().toSeq shouldBe Seq(1, 2, 3, 5, 6)
    tree.iterator(false).toSeq shouldBe Seq(1, 2, 3, 5, 6)
  }
  it should "work correctly for UnvaluedBinaryTree type 2/3" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(4))) + Leaf(3)
    tree shouldBe UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(2), Leaf(3)), Leaf(4)))
  }
  it should "work correctly for UnvaluedBinaryTree type 4" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(3))) + Leaf(4)
    tree.iterator().toList shouldBe List(1, 2, 3, 4)
    tree shouldBe UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(2), Leaf(3)), Leaf(4)))
  }
  it should "work correctly for UnvaluedBinaryTree type 5" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(4))) + Leaf(0)
    tree shouldBe UnvaluedBinaryTree(Leaf(0), UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(4))))
  }
  ignore should "work correctly for BinaryTree" in {
    import BinaryTree._
    val tree = BinaryTree(1, Leaf(0), Leaf(2)) + Leaf(3)
    tree shouldBe UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(0), Leaf(1)), UnvaluedBinaryTree(Leaf(2), Leaf(4)))
  }

  behavior of "+(value)"

  it should "work correctly for GeneralTree" in {
    import GeneralTree._
    val tree = GeneralTree(0, Nil) + Leaf(1) + Leaf(2) + Leaf(3) + Leaf(4)
    tree shouldBe GeneralTree(0, List(Leaf(1), Leaf(2), Leaf(3), Leaf(4)))
  }
  it should "work correctly for UnvaluedBinaryTree type 1/3" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6))) + Leaf(2)
    tree.iterator().toList shouldBe List(1, 2, 3, 5, 6)
  }

  it should "work correctly for UnvaluedBinaryTree type 2/3" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(4))) + 3
    tree shouldBe UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(2), Leaf(3)), Leaf(4)))
  }
  it should "work correctly for UnvaluedBinaryTree type 4" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), Empty) + Leaf(2) + Leaf(3) + Leaf(4)
    tree.iterator().toList shouldBe List(1, 2, 3, 4)
  }
  it should "work correctly for UnvaluedBinaryTree type 5" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(4))) + 0
    tree shouldBe UnvaluedBinaryTree(Leaf(0), UnvaluedBinaryTree(Leaf(1), UnvaluedBinaryTree(Leaf(2), Leaf(4))))
  }
  it should "work correctly for GeneralTree with parent insertion" in {
    implicit object GeneralTreeBuilderInt extends GeneralTreeBuilder[Int] {
      def getParent(tree: Tree[Int], t: Int): Option[Node[Int]] = tree.find(t / 10)
    }
  }

  behavior of "size"
  it should "work for GeneralTree" in {
    import GeneralTree._
    val tree = GeneralTree(0, Nil) + 1 + 2 + 3 + 4
    tree.size shouldBe 5
  }
  it should "work for UnvaluedBinaryTree" in {
    import UnvaluedBinaryTree._
    val tree = UnvaluedBinaryTree(Leaf(1), Empty) + Leaf(2) + Leaf(3) + Leaf(4)
    tree.size shouldBe 4
  }

  behavior of "get"
  it should "work for GeneralTree" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.get should matchPattern { case Some(1) => }
  }
  it should "work for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.get should matchPattern { case None => }
  }

  behavior of "depth"
  it should "work for GeneralTree" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.depth shouldBe 2
  }
  it should "work for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.depth shouldBe 3
  }

  behavior of "compareValues"
  it should "work for Leaf" in {
    val tree1 = GeneralTree(1, List(Leaf(2), Leaf(3)))
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    tree1.compareValues(leaf1) should matchPattern { case Kleenean(Some(true)) => }
    leaf1.compareValues(tree1) should matchPattern { case Kleenean(Some(true)) => }
    tree1.compareValues(leaf2) should matchPattern { case Kleenean(Some(false)) => }
    leaf2.compareValues(tree1) should matchPattern { case Kleenean(Some(false)) => }
  }

  behavior of "like"
  it should "work for GeneralTree" in {
    val tree1 = GeneralTree(1, List(Leaf(2), Leaf(3)))
    val tree2 = GeneralTree(1, List(Leaf(5), Leaf(6)))
    //noinspection ScalaUnusedSymbol
    val tree3 = GeneralTree(1, List(GeneralTree(4, List(Leaf(5), Leaf(6)))))
    //noinspection ScalaUnusedSymbol
    val tree4 = GeneralTree(1, List(GeneralTree(4, List(Leaf(10), Leaf(11)))))
    tree1.like(tree2) should matchPattern { case Kleenean(Some(false)) => }
    // CONSIDER understand why this doesn't work. At appears to yield Some(true)
    //    tree3.like(tree4) should matchPattern {case Kleenean(Some(true)) => }
  }
  it should "work for UnvaluedBinaryTree" in {
    val tree1 = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    val tree2 = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    // the like method doesn't really make sense for Binary Trees
    tree1 like tree2 should matchPattern { case Kleenean(None) => }
  }

  behavior of "includes"
  it should "work for GeneralTree with Node" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.includes(Leaf(1)) shouldBe true
    tree.includes(Leaf(2)) shouldBe true
    tree.includes(Leaf(3)) shouldBe true
    tree.includes(Leaf(4)) shouldBe false
    tree.includes(Leaf(0)) shouldBe false
  }
  it should "work for UnvaluedBinaryTree with Node" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.includes(Leaf(1)) shouldBe true
    tree.includes(Leaf(2)) shouldBe false
    tree.includes(Leaf(3)) shouldBe true
    tree.includes(Leaf(4)) shouldBe false
    tree.includes(Leaf(5)) shouldBe true
    tree.includes(Leaf(6)) shouldBe true
  }
  behavior of "includes"
  it should "work for GeneralTree" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.includesValue(1) shouldBe true
    tree.includesValue(2) shouldBe true
    tree.includesValue(3) shouldBe true
    tree.includesValue(4) shouldBe false
    tree.includesValue(0) shouldBe false
  }
  it should "work for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.includesValue(1) shouldBe true
    tree.includesValue(2) shouldBe false
    tree.includesValue(3) shouldBe true
    tree.includesValue(4) shouldBe false
    tree.includesValue(5) shouldBe true
    tree.includesValue(6) shouldBe true
  }
  it should "work for GeneralTree using the two-parameter form" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.includes(1, 1) shouldBe true
    tree.includes(1, 2) shouldBe true
    tree.includes(1, 3) shouldBe true
    tree.includes(2, 3) shouldBe false
    tree.includes(2, 1) shouldBe false
    tree.includesValue(0) shouldBe false
  }

  behavior of "find"
  it should "work for GeneralTree" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.find(1) should matchPattern { case Some(GeneralTree(1, Seq(Leaf(2), Leaf(3)))) => }
    tree.find(2) should matchPattern { case Some(Leaf(2)) => }
    tree.find(3) should matchPattern { case Some(Leaf(3)) => }
    tree.find(4) should matchPattern { case None => }
    tree.find(0) should matchPattern { case None => }
  }
  it should "work for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.find(1) should matchPattern { case Some(Leaf(1)) => }
    tree.find(2) should matchPattern { case None => }
    tree.find(3) should matchPattern { case Some(Leaf(3)) => }
    tree.find(4) should matchPattern { case None => }
    tree.find(5) should matchPattern { case Some(Leaf((5))) => }
    tree.find(6) should matchPattern { case Some(Leaf((6))) => }
  }

  behavior of "filter"
  it should "work for GeneralTree" in {
    val tree = GeneralTree(1, List(Leaf(2), Leaf(3)))
    tree.filter(_.get.get % 2 == 1).toList shouldBe List(Leaf(3), GeneralTree(1, List(Leaf(2), Leaf(3))))
  }
  it should "work for UnvaluedBinaryTree" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.filter(_.get match {
      case Some(x) => x % 2 == 1
      case _ => false
    }).toList shouldBe Seq(Leaf(1), Leaf(3), Leaf(5))
  }

  // CONSIDER try to understand why this bit of code -- which hasn't actually changed -- suddenly causes a weird compiler error
  //  it should "work correctly for unsorted Flatland tree" in {
  //    val uo = Option(getClass.getResource("flatland.txt"))
  //    uo should matchPattern { case Some(_) => }
  //    val so = uo map (_.openStream)
  //    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
  //    import scala.language.postfixOps
  //    val z: Seq[String] = wso match {
  //      case Some(ws) => ws map (_.toLowerCase) filterNot (_.isEmpty) distinct
  //      case _ => Seq[String]()
  //    }
  //    import UnvaluedBinaryTree._
  //      implicit object StringStringValueOps$ extends StringValueOps[String] {
  //        def getParentKey(v: String): Option[String] = Some(v.substring(0,v.length-1))
  //        def createValueFromKey(k: String): Option[String] = Some(k)
  //      }
  //    val tree = Spy.noSpy(Tree.populateOrderedTree(z))
  //    val strings = tree.iterator().take(10).toList
  //    strings shouldBe List("a", "about", "above", "actually", "ago", "alas", "all", "and", "another", "anything")
  //  }

  behavior of "UnvaluedBinaryTree"
  it should "apply correctly with varargs" in {
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    tree.includesValue(1) shouldBe true
    tree.depth shouldBe 3
    tree.render() shouldBe "--(\n  --(\n    1,\n    3\n  ),\n  --(\n    5,\n    6\n  )\n)"
  }

  behavior of "createIndexedTree"
  it should "work for UnvaluedBinaryTree" in {
    // TODO there is definitely something wrong with the indexes for a Binary tree, although the GeneralTree is fine.
    val tree = UnvaluedBinaryTree(UnvaluedBinaryTree(Leaf(1), Leaf(3)), UnvaluedBinaryTree(Leaf(5), Leaf(6)))
    val indexedTree = Tree.createIndexedTree(tree).asInstanceOf[IndexedTree[Int]]
    indexedTree.lIndex should matchPattern { case Some(0) => }
    indexedTree.rIndex should matchPattern { case Some(9) => }
    val one: Option[IndexedNode[Int]] = indexedTree.find(1).asInstanceOf[Option[IndexedNode[Int]]]
    one.get.lIndex should matchPattern { case Some(2) => }
    one.get.rIndex should matchPattern { case Some(3) => }
    indexedTree.find(5).asInstanceOf[Option[IndexedNode[Int]]].get.lIndex should matchPattern { case Some(6) => }
    indexedTree.find(6).asInstanceOf[Option[IndexedNode[Int]]].get.rIndex should matchPattern { case Some(9) => }
  }
  it should "work for GeneralTree[Int]" in {
    import GeneralTree._
    val tree = GeneralTree(0, Nil) + 1 + 3 + 5 + 6
    val indexedTree = Tree.createIndexedTree(tree).asInstanceOf[IndexedTree[Int]]
    indexedTree.lIndex should matchPattern { case Some(0) => }
    indexedTree.rIndex should matchPattern { case Some(9) => }
    val one: Option[IndexedNode[Int]] = indexedTree.find(1).asInstanceOf[Option[IndexedNode[Int]]]
    one.get.lIndex should matchPattern { case Some(1) => }
    one.get.rIndex should matchPattern { case Some(2) => }
    indexedTree.find(5).asInstanceOf[Option[IndexedNode[Int]]].get.lIndex should matchPattern { case Some(5) => }
    indexedTree.find(6).asInstanceOf[Option[IndexedNode[Int]]].get.rIndex should matchPattern { case Some(8) => }

  }
  /**
    * CONSIDER reimplement this using GeneralTree instead of GeneralKVTree.
    * XXX I think it should work -- it just requires not importing GeneralTree._ which gives non-standard behavior (we should fix that too)
    *
    * This test is based on the description of the "nested set model" in Wikipedia: https://en.wikipedia.org/wiki/Nested_set_model
    *
    * There is in fact a slight difference: in the following code, children are forced to be alphabetical, whereas that's not so in the Wikipedia article.
    * Also, we start counting at 0, whereas Wikipedia starts at 1.
    */
  it should "work for GeneralKVTree[String]" in {
    val clothing = "Clothing"
    val mens = "Men's"
    val womens = "Women's"
    val blouses = "Blouses"
    val skirts = "Skirts"
    val dresses = "Dresses"
    val suits = "Suits"
    val sunDresses = "Sun Dresses"
    val eveningGowns = "Evening Gowns"
    val jackets = "Jackets"
    val slacks = "Slacks"
    val racks = Map(mens -> clothing, womens -> clothing, dresses -> womens, blouses -> womens, skirts -> womens, suits -> mens, sunDresses -> dresses, eveningGowns -> dresses, slacks -> suits, jackets -> suits)
    implicit object ClothingValueOps extends StringValueOps[String] {
      def getParentKey(v: String): Option[String] = racks.get(v)

      def createValueFromKey(k: String, vo: => Option[String]): Option[String] = Some(k)
    }
    implicit object GeneralKVTreeBuilderString extends GeneralKVTreeBuilder[String, String]
    def add(t: GeneralKVTree[String, String], s: String): GeneralKVTree[String, String] = (t :+ s).asInstanceOf[GeneralKVTree[String, String]]

    val root = GeneralKVTree(Some(clothing), Nil)
    root.key shouldBe Some("Clothing")

    val tree: GeneralKVTree[String, String] = List(mens, womens, dresses, skirts, blouses, suits, eveningGowns, sunDresses, slacks, jackets).foldLeft(root)(add)
    val nMens = tree.findByKey("Men's")
    nMens should matchPattern { case Some(_) => }
    nMens.get should matchPattern { case GeneralKVTree(_, _) => }
    nMens.get.get should matchPattern { case Some("Men's") => }
    val indexedTree = Tree.createIndexedTree(tree).asInstanceOf[IndexedTree[String]]
    indexedTree.lIndex should matchPattern { case Some(0) => }
    indexedTree.rIndex should matchPattern { case Some(21) => }
    val inMens: Option[IndexedNode[String]] = indexedTree.find(mens).asInstanceOf[Option[IndexedNode[String]]]
    inMens.get.lIndex should matchPattern { case Some(1) => }
    inMens.get.rIndex should matchPattern { case Some(8) => }
    indexedTree.find(dresses).get.asInstanceOf[IndexedNode[String]].lIndex should matchPattern { case Some(12) => }
    indexedTree.find(jackets).get.asInstanceOf[IndexedNode[String]].rIndex should matchPattern { case Some(4) => }

  }
  val flatLandTree = "a{{about}above{{actually}ago{{alas}all{{and}another{{anything}appear{{are}as{{at}back{{be}because{{become}becoming{{been}below{{bringing}but{{by}call{{can}ceased{{circle}clearer{{condition}contrary{{correct}could{{country}countrymen{{dare}demonstrate{{described}distinguish{{down}drawing{{edge}edges{{exactly}except{{eye}far{{few}figure{{figures}find{{fixed}flatland{{flatlander}freely{{from}gradually{{happy}hard{{has}have{{hexagons}higher{{i}imagine{{impossible}in{{inhabitants}instead{{into}is{{it}its{{kind}last{{leaning}least{{like}line{{lines}live{{look}lower{{luminous}make{{middle}mind{{more}move{{moving}much{{my}nature{{necessity}nor{{not}nothing{{notion}now{{of}on{{once}one{{only}opened{{or}other{{our}oval{{over}p{{paper}penny{{pentagons}perceive{{place}placed{{places}power{{pretty}privileged{{readers}remaining{{rising}said{{say}see{{shadows}sheet{{should}sight{{sinking}so{{solid}space{{speedily}squares{{straight}such{{suppose}surface{{table}tables{{that}the{{their}them{{then}there{{things}this{{thus}to{{triangles}universe{{upon}us{{vast}very{{view}views{{visible}was{{we}were{{what}when{{which}who{{will}with{{without}world{{years}you{{your}yourself}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
}
