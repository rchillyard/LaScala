package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.HasKey
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class MPTTSpec extends FlatSpec with Matchers {

  abstract class HasKeyString extends HasKey[String] {
    type K = String
    def getKey(x: String): K = x
  }
  implicit object HasKeyString extends HasKeyString

  trait OrderingValueString extends Ordering[Value[String]] {
    def compare(x: Value[String], y: Value[String]): Int = x.value.compareTo(y.value)
  }
  implicit object OrderingValueString extends OrderingValueString

  behavior of "MPTTEntry.contains"
  it should "be true for (0,3).contains((1,2)" in {
    val m1 = MPTTEntry("A")(0,3)
    val m2 = MPTTEntry("Aardvark")(1,2)
    m1.contains(m2) shouldBe true
  }
  it should "be true for (0,1).contains((0,1)" in {
    val m1 = MPTTEntry("A")(0,1)
    m1.contains(m1) shouldBe true
  }
  it should "be false for (1,2).contains((0,3)" in {
    val m1 = MPTTEntry("A")(0,3)
    val m2 = MPTTEntry("Aardvark")(1,2)
    m2.contains(m1) shouldBe false
  }

  behavior of "real-life GeneralTree"
  it should "build correctly" in {
    implicit object HasKeyInt extends HasKey[Int] {
      type K = String
      def getKey(v: Int): String = v.toString
    }
    trait NodeTypeParentInt extends HasParent[Int] {
      def createParent(a: Int): Option[Node[Int]] = None
      def getParentKey(a: Int): Option[String] = Some(a./(10).toString)
    }
    val tree = GeneralTree(0, Seq(GeneralTree(1, Seq(Leaf(11), Leaf(12), Leaf(13), Leaf(14))), GeneralTree(2, Seq(Leaf(21), Leaf(22), Leaf(23))), Leaf(3), Leaf(4)))
    val indexedTree = Tree.createIndexedTree(tree)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[Int]])
    mptt.contains("0","0") should matchPattern { case Some(true) => }
    mptt.contains("0","1") should matchPattern { case Some(true) => }
    mptt.contains("0","11") should matchPattern { case Some(true) => }
    mptt.contains("0","14") should matchPattern { case Some(true) => }
    mptt.contains("0","2") should matchPattern { case Some(true) => }
    mptt.contains("0","21") should matchPattern { case Some(true) => }
    mptt.contains("0","23") should matchPattern { case Some(true) => }
    mptt.contains("0","3") should matchPattern { case Some(true) => }
    mptt.contains("0","4") should matchPattern { case Some(true) => }
    mptt.contains("1","0") should matchPattern { case Some(false) => }
    mptt.contains("1","1") should matchPattern { case Some(true) => }
    mptt.contains("1","11") should matchPattern { case Some(true) => }
    mptt.contains("1","14") should matchPattern { case Some(true) => }
    mptt.contains("1","2") should matchPattern { case Some(false) => }
    mptt.contains("1","21") should matchPattern { case Some(false) => }
    mptt.contains("1","23") should matchPattern { case Some(false) => }
    mptt.contains("1","3") should matchPattern { case Some(false) => }
    mptt.contains("1","4") should matchPattern { case Some(false) => }
    mptt.contains("2","0") should matchPattern { case Some(false) => }
    mptt.contains("2","1") should matchPattern { case Some(false) => }
    mptt.contains("2","11") should matchPattern { case Some(false) => }
    mptt.contains("2","14") should matchPattern { case Some(false) => }
    mptt.contains("2","2") should matchPattern { case Some(true) => }
    mptt.contains("2","21") should matchPattern { case Some(true) => }
    mptt.contains("2","23") should matchPattern { case Some(true) => }
    mptt.contains("2","3") should matchPattern { case Some(false) => }
    mptt.contains("2","4") should matchPattern { case Some(false) => }
    mptt.contains("3","0") should matchPattern { case Some(false) => }
    mptt.contains("3","1") should matchPattern { case Some(false) => }
    mptt.contains("3","11") should matchPattern { case Some(false) => }
    mptt.contains("3","14") should matchPattern { case Some(false) => }
    mptt.contains("3","2") should matchPattern { case Some(false) => }
    mptt.contains("3","21") should matchPattern { case Some(false) => }
    mptt.contains("3","23") should matchPattern { case Some(false) => }
    mptt.contains("3","3") should matchPattern { case Some(true) => }
    mptt.contains("3","4") should matchPattern { case Some(false) => }
  }
    behavior of "real-life UnvaluedBinaryTree"
  ignore should "build correctly" in {
    val uo = Option(getClass.getResource("flatland.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map {_.openStream}
    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
    import scala.language.postfixOps
    val z: Seq[String] = wso match {
      case Some(ws) => ws map {_.toLowerCase} filterNot {_.isEmpty} distinct
      case _ => Seq[String]()
    }
    import UnvaluedBinaryTree._
    trait OrderingValueString extends Ordering[Value[String]] {
      def compare(x: Value[String], y: Value[String]): Int = x.value.compareTo(y.value)
    }
    implicit object OrderingValueString extends OrderingValueString
    implicit object UnvaluedBinaryTreeBuilderValueString extends UnvaluedBinaryTreeBuilder[Value[String]]
    trait ValueNodeParent extends NodeParent[Value[String]] {
      // XXX: this doesn't really make sense for a binary tree where nodes don't typically have values!
      def isParent(parent: Node[Value[String]], child: Node[Value[String]]): Boolean = false
    }
    implicit object ValueNodeParent extends ValueNodeParent
    trait ValueHasParent extends HasParent[Value[String]] {
      // XXX: this doesn't really make sense for a binary tree where nodes don't typically have values!
      def getParentKey(t: Value[String]): Option[String] = None
      def createParent(t: Value[String]): Option[Node[Value[String]]] = None
    }
    implicit object ValueHasParent extends ValueHasParent
    implicit object HasKeyValueString extends HasKey[Value[String]] {
      type K = String
      def getKey(v: Value[String]): String = v.value
    }
    val tree = Tree.populateOrderedTree(z map(Value(_)))
    val mptt = MPTT.createValuedMPTT(Tree.createIndexedTree(tree.asInstanceOf[UnvaluedBinaryTree[Value[String]]]))
    mptt.index.size shouldBe 177

    println(mptt)

    println(mptt.index.keySet)

    // First we do it the slow way
    val meo = tree.find("flatland")
    println(s"flatland tree: ${meo.get}")
//    meo.exists(_.includes("flatlander")) shouldBe true
//
//    // Next we do it the fast way
//    mptt.contains("flatland","flatlander") shouldBe Some(true)
//
//    // And again...
//    mptt.contains("i","flatlander") shouldBe Some(true)
//
//    testMpttLookup(mptt,"i", "flatlander") shouldBe true
//
//    // this is failing
////    testMpttLookup(mptt,"edges","surface") should matchPattern { case Some(true) => }
//
//    val x = mptt.index.keySet
//    val xSorted = SortedSet[String]() ++ x
//
//    def check(a: (String, String)): Boolean = !testMpttLookup(mptt, a._1, a._2)
//    println((x zip xSorted))
//    val r = (x zip xSorted) find (check)
//    r shouldBe None
  }

  def testMpttLookup(mPTT: MPTT[String], x: String, y: String): Boolean = x.contains(y) == (x.compareTo(y)<1)
}
