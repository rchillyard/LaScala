package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.HasKey
import org.scalatest.{FlatSpec, Matchers}

/**
  * TODO there's a lot of redundant tests here -- clean them out
  *
  * Created by scalaprof on 10/19/16.
  */
class KVTreeSpec extends FlatSpec with Matchers {

  //  Spy.spying = true

  abstract class HasKeyString extends HasKey[String] {
    type K = String

    def getKey(x: String): K = x
  }

  implicit object HasKeyString extends HasKeyString

  abstract class HasKeyInt extends HasKey[Int] {
    type K = String

    def getKey(x: Int): K = x.toString
  }

  implicit object HasKeyInt extends HasKeyInt

  implicit object GeneralKVTreeBuilderStringInt extends GeneralKVTreeBuilder[Int]

  implicit object ValueBuilderNodeType extends ValueBuilder[Int] {
    def buildValue(k: String): Value[Int] = Value[Int](k.toInt)
  }

  implicit object NodeTypeParent extends HasParent[Value[Int]] {
    // Here, we take the Int value, modulo 10, and turn that into a String
    def getParentKey(t: Value[Int]): Option[String] = Some(t.value./(10).toString)

    def createParent(t: Value[Int]): Option[Tree[Value[Int]]] = {
      val treeBuilder = implicitly[TreeBuilder[Value[Int]]]
      val vo = for (k <- getParentKey(t)) yield implicitly[ValueBuilder[Int]].buildValue(k)
      for (_ <- vo) yield treeBuilder.buildTree(vo, Seq())
    }
  }

  implicit object NodeTypeNodeParent extends NodeParent[Value[Int]] {
    def isParent(parent: Node[Value[Int]], child: Node[Value[Int]]): Boolean = parent match {
      case Branch(ns) => ns.contains(child)
      case _ => false
    }
  }

  behavior of "nodeIterator"

  it should "work properly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    val ns = tree.nodeIterator()
    ns.size shouldBe 4
  }

  behavior of "iterator"

  it should "work properly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    val ns = tree.iterator(false).toList
    ns.size shouldBe 4
    ns shouldBe List(Value(0), Value(1), Value(2), Value(3))
  }

  behavior of ":+(node)"

  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    val tree2 = tree :+ Leaf(Value[Int](4))
    val ns = tree2.iterator(false).toList
    ns.size shouldBe 5
    ns shouldBe List(Value(0), Value(1), Value(2), Value(3), Value(4))
  }

  behavior of ":+(value)"

  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    val tree2 = tree :+ Value[Int](4)
    val ns = tree2.iterator(false).toList
    ns.size shouldBe 5
    ns shouldBe List(Value(0), Value(1), Value(2), Value(3), Value(4))
  }


  behavior of "size"
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    tree.size shouldBe 4
  }

  behavior of "get"
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    tree.get should matchPattern { case Some(Value(0)) => }
  }

  behavior of "depth"
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    tree.depth shouldBe 2
  }

  behavior of "includes"
  it should "work for GeneralKVTree" in {
    val tree = GeneralKVTree[Int](Some(Value(1)), Seq(Leaf(Value(2)), Leaf(Value(3))))
    tree.includesValue(Value(1)) shouldBe true
    tree.includesValue(Value(2)) shouldBe true
    tree.includesValue(Value(3)) shouldBe true
    tree.includesValue(Value(4)) shouldBe false
    tree.includesValue(Value(0)) shouldBe false
  }

  behavior of "find"
  it should "work for GeneralKVTree" in {
    val tree = GeneralKVTree[Int](Some(Value(1)), Seq(Leaf(Value(2)), Leaf(Value(3))))
    tree.find(Value(1)) should matchPattern { case Some(GeneralKVTree(Some(Value(1)), Seq(Leaf(Value(2)), Leaf(Value(3))))) => }
    tree.find(Value(2)) should matchPattern { case Some(Leaf(Value(2))) => }
    tree.find(Value(3)) should matchPattern { case Some(Leaf(Value(3))) => }
    tree.find(Value(4)) should matchPattern { case None => }
    tree.find(0) should matchPattern { case None => }
  }

  behavior of ":+, findParent, createParent, etc."
  it should "work correctly for GeneralKVTree" in {
    val tree = GeneralKVTree(Some(Value[Int](0)), Seq(Leaf(Value[Int](1)), Leaf(Value[Int](2)), Leaf(Value[Int](3))))
    val tree2 = tree :+ Value[Int](14)
    val ns = tree2.iterator(false).toList
    ns.size shouldBe 5
    ns shouldBe List(Value(0), Value(1), Value(14), Value(2), Value(3))
  }

  val flatLandTree = "a{{about}above{{actually}ago{{alas}all{{and}another{{anything}appear{{are}as{{at}back{{be}because{{become}becoming{{been}below{{bringing}but{{by}call{{can}ceased{{circle}clearer{{condition}contrary{{correct}could{{country}countrymen{{dare}demonstrate{{described}distinguish{{down}drawing{{edge}edges{{exactly}except{{eye}far{{few}figure{{figures}find{{fixed}flatland{{flatlander}freely{{from}gradually{{happy}hard{{has}have{{hexagons}higher{{i}imagine{{impossible}in{{inhabitants}instead{{into}is{{it}its{{kind}last{{leaning}least{{like}line{{lines}live{{look}lower{{luminous}make{{middle}mind{{more}move{{moving}much{{my}nature{{necessity}nor{{not}nothing{{notion}now{{of}on{{once}one{{only}opened{{or}other{{our}oval{{over}p{{paper}penny{{pentagons}perceive{{place}placed{{places}power{{pretty}privileged{{readers}remaining{{rising}said{{say}see{{shadows}sheet{{should}sight{{sinking}so{{solid}space{{speedily}squares{{straight}such{{suppose}surface{{table}tables{{that}the{{their}them{{then}there{{things}this{{thus}to{{triangles}universe{{upon}us{{vast}very{{view}views{{visible}was{{we}were{{what}when{{which}who{{will}with{{without}world{{years}you{{your}yourself}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
}
