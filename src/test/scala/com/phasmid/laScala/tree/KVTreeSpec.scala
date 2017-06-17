package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

/**
  * CONSIDER there's a lot of redundant tests here -- clean them out
  *
  * Created by scalaprof on 10/19/16.
  */
class KVTreeSpec extends FlatSpec with Matchers {

  implicit object IntStringValueOps$ extends StringValueOps[Int] {
    def getParentKey(t: Int): Option[String] = (for (i <- Try(t / 10); s = i.toString) yield s).toOption

    def createValueFromKey(k: String, vo: => Option[Int]): Option[Int] = Try(k.toInt).toOption
  }

  implicit object GeneralKVTreeBuilderStringInt extends GeneralKVTreeBuilder[String, Int]

  behavior of "nodeIterator"

  it should "work properly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    val ns = tree.nodeIterator()
    ns.size shouldBe 4
  }

  behavior of "iterator"

  it should "work properly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    val ns = tree.iterator(false).toList
    ns.size shouldBe 4
    ns shouldBe List(0, 1, 2, 3)
  }

  behavior of ":+(node)"

  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    val tree2 = tree :+ Leaf(4)
    val ns = tree2.iterator(false).toList
    ns.size shouldBe 5
    ns shouldBe List(0, 1, 2, 3, 4)
  }

  behavior of ":+(value)"

  it should "work correctly for GeneralKVTree(2)" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    val tree2 = tree :+ 4
    val ns = tree2.iterator(false).toList
    ns.size shouldBe 5
    ns shouldBe List(0, 1, 2, 3, 4)
  }

  behavior of ":+(node)"

  it should "work correctly for GeneralKVTree(1)" in {
    val tree: Tree[Int] = GeneralKVTree(Some(0), Nil) :+ Leaf(1)
    tree shouldBe GeneralKVTree(Some(0), List(Leaf(1)))
    tree.size shouldBe 2
    tree.depth shouldBe 2
    tree.iterator().toSeq shouldBe Seq(1, 0)
    tree.iterator(false).toSeq shouldBe Seq(0, 1)
    tree.render() shouldBe "0--(1)"
  }

  it should "work correctly for GeneralKVTree(2)" in {
    val tree: Tree[Int] = GeneralKVTree(Some(0), Nil) :+ Leaf(1) :+ Leaf(2)
    tree shouldBe GeneralKVTree(Some(0), List(Leaf(1), Leaf(2)))
    tree.size shouldBe 3
    tree.depth shouldBe 2
    tree.iterator(false).toSeq shouldBe Seq(0, 1, 2)
    tree.iterator().toSeq shouldBe Seq(1, 2, 0)
    tree.render() shouldBe "0--(\n  1,\n  2\n)"
  }

  it should "work correctly for GeneralKVTree(3)" in {
    val tree: Tree[Int] = GeneralKVTree(Some(0), Nil) :+ Leaf(1) :+ Leaf(12)
    tree.size shouldBe 3
    tree.depth shouldBe 3
    tree shouldBe GeneralKVTree(Some(0), List(GeneralKVTree(Some(1), List(Leaf(12)))))
    tree.iterator(false).toSeq shouldBe Seq(0, 1, 12)
    tree.iterator().toSeq shouldBe Seq(12, 1, 0)
    tree.render() shouldBe "0--(1--(12))"
  }

  behavior of "size"
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    tree.size shouldBe 4
  }

  behavior of "get"
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    tree.get should matchPattern { case Some(0) => }
  }

  behavior of "depth"
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    tree.depth shouldBe 2
  }

  behavior of "includes"
  it should "work for GeneralKVTree" in {
    val tree = GeneralKVTree[String, Int](Some(1), List(Leaf(2), Leaf(3)))
    tree.includesValue(1) shouldBe true
    tree.includesValue(2) shouldBe true
    tree.includesValue(3) shouldBe true
    tree.includesValue(4) shouldBe false
    tree.includesValue(0) shouldBe false
  }

  behavior of "find"
  it should "work for GeneralKVTree" in {
    val tree = GeneralKVTree[String, Int](Some(1), List(Leaf(2), Leaf(3)))
    tree.find(1) should matchPattern { case Some(GeneralKVTree(Some(1), Seq(Leaf(2), Leaf(3)))) => }
    tree.find(2) should matchPattern { case Some(Leaf(2)) => }
    tree.find(3) should matchPattern { case Some(Leaf(3)) => }
    tree.find(4) should matchPattern { case None => }
    tree.find(0) should matchPattern { case None => }
  }

  behavior of ":+, findParent, createValueFromKey, etc."
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(0), List(Leaf(1), Leaf(2), Leaf(3)))
    val tree2 = tree :+ 14
    val ns = tree2.iterator(false).toList
    ns.size shouldBe 5
    ns shouldBe List(0, 1, 14, 2, 3)
  }

  behavior of "createIndexedTree"
  /**
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
    val tree = List(mens, womens, dresses, skirts, blouses, suits, eveningGowns, sunDresses, slacks, jackets).foldLeft(root)(add)
    val indexedTree = Tree.createIndexedTree(tree).asInstanceOf[IndexedTree[String]]
    indexedTree.lIndex should matchPattern { case Some(0) => }
    indexedTree.rIndex should matchPattern { case Some(21) => }
    val nMens: Option[IndexedNode[String]] = indexedTree.find(mens).asInstanceOf[Option[IndexedNode[String]]]
    nMens.get.lIndex should matchPattern { case Some(1) => }
    nMens.get.rIndex should matchPattern { case Some(8) => }
    indexedTree.find(dresses).get.asInstanceOf[IndexedNode[String]].lIndex should matchPattern { case Some(12) => }
    indexedTree.find(jackets).get.asInstanceOf[IndexedNode[String]].rIndex should matchPattern { case Some(4) => }

  }

  //  val flatLandTree = "a{{about}above{{actually}ago{{alas}all{{and}another{{anything}appear{{are}as{{at}back{{be}because{{become}becoming{{been}below{{bringing}but{{by}call{{can}ceased{{circle}clearer{{condition}contrary{{correct}could{{country}countrymen{{dare}demonstrate{{described}distinguish{{down}drawing{{edge}edges{{exactly}except{{eye}far{{few}figure{{figures}find{{fixed}flatland{{flatlander}freely{{from}gradually{{happy}hard{{has}have{{hexagons}higher{{i}imagine{{impossible}in{{inhabitants}instead{{into}is{{it}its{{kind}last{{leaning}least{{like}line{{lines}live{{look}lower{{luminous}make{{middle}mind{{more}move{{moving}much{{my}nature{{necessity}nor{{not}nothing{{notion}now{{of}on{{once}one{{only}opened{{or}other{{our}oval{{over}p{{paper}penny{{pentagons}perceive{{place}placed{{places}power{{pretty}privileged{{readers}remaining{{rising}said{{say}see{{shadows}sheet{{should}sight{{sinking}so{{solid}space{{speedily}squares{{straight}such{{suppose}surface{{table}tables{{that}the{{their}them{{then}there{{things}this{{thus}to{{triangles}universe{{upon}us{{vast}very{{view}views{{visible}was{{we}were{{what}when{{which}who{{will}with{{without}world{{years}you{{your}yourself}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
}
