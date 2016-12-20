package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.{Spy}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class MPTTSpec extends FlatSpec with Matchers {

//  abstract class HasKeyString extends HasKey[String] {
//    type K = String
//    def getKey(x: String): K = x
//
//    def createValueFromKey(w: String): String = w
//  }
//  implicit object HasKeyString extends HasKeyString

//  trait OrderingValueString extends Ordering[String] {
//    def compare(x: String, y: String): Int = x.value.compareTo(y.value)
//  }
//  implicit object OrderingValueString extends OrderingValueString

  implicit object StringStringKeyOps extends KeyOps[String,String] {
    def getKeyFromValue(v: String): String = v
    def getParentKey(a: String): Option[String] = Some(a.substring(0,a.length-1))
  }

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
//    implicit object HasKeyInt extends HasKey[Int] {
//      def createValueFromKey(w: String): Int = ??? // FIXME
//
//      type K = String
//      def getKey(v: Int): String = v.toString
//    }
    trait StringIntHasParent extends HasParent[String,Int] {
      def createParent(a: Int): Option[Node[Int]] = None
      def getParentKey(a: Int): Option[String] = Some(a./(10).toString)
    }
    implicit object StringIntKeyOps extends KeyOps[String,Int] {
      def getParentKey(a: Int): Option[String] = Some(a./(10).toString)
      def getKeyFromValue(v: Int): String = v.toString
    }
    val tree = GeneralTree(0, Seq(GeneralTree(1, Seq(Leaf(11), Leaf(12), Leaf(13), Leaf(14))), GeneralTree(2, Seq(Leaf(21), Leaf(22), Leaf(23))), Leaf(3), Leaf(4)))
    val indexedTree = Tree.createIndexedTree(tree)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[Int]])
    for (i <- mptt.index) println(s"${i._2}")
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
    import UnvaluedBinaryTree._
    trait OrderingValueString extends Ordering[String] {
      def compare(x: String, y: String): Int = x.compareTo(y)
    }
    implicit object OrderingValueString extends OrderingValueString
//    implicit object UnvaluedBinaryTreeBuilderValueString extends UnvaluedBinaryTreeBuilder[String]
//    trait ValueNodeParent extends NodeParent[String] {
//      // XXX: this doesn't really make sense for a binary tree where nodes don't typically have values!
//      def isParent(parent: Node[String], child: Node[String]): Boolean = false
//    }
//    implicit object ValueNodeParent extends ValueNodeParent

//    trait ValueHasParent extends HasParent[String,String] {
//      def getParent(tree: Tree[String], t: String): Option[Node[String]] = ???
//
//      def createValueFromKey(k: String): String = k
//
//      // XXX: this doesn't really make sense for a binary tree where nodes don't typically have values!
//      def getParentKey(t: String): Option[String] = None
//      def createParent(t: String): Option[Node[String]] = None
//    }
//    implicit object ValueHasParent extends ValueHasParent
//    implicit object HasKeyValueString extends HasKey[String] {
//      def createValueFromKey(w: String): String = Value(w)
//
//      type K = String
//      def getKey(v: String): String = v.value
//    }

//    implicit object StringValueKeyOps extends KeyOps[String,String] {
//      def getParentKey(t: String): String = ???
//
//      def getKey(t: String): String = ???
//
//      def createValueFromKey(k: String): String = ???
//    }
    implicit object StringStringHasParent extends HasParent[String,String] {
  def getParentKey(t: String): Option[String] = ???
  def createValueFromKey(k: String): String = ???
}
    val tree = Spy.noSpy(Tree.populateOrderedTree(z))
    val mptt = MPTT.createValuedMPTT(Tree.createIndexedTree(tree.asInstanceOf[UnvaluedBinaryTree[String]]))
    mptt.index.size shouldBe 176

    println(mptt)

    println(mptt.index.keySet)

    val nodes = tree.nodeIterator().filter(_.isLeaf).toList
    println(nodes)
    val flatland = nodes.find(_.get.contains(("flatland")))
    flatland should matchPattern { case Some(_) => }
    flatland match {
      case Some(node) => node.includesValue(("flatland")) shouldBe true
      case _ => fail("logic")
    }

    // NOTE: the logic that follows here doesn't make any sense for an UnvaluedBinaryTree -- it might make sense for a BinaryTree
//    // First we do it the slow way
//    val meo = tree.find(Value("flatland"))
//    meo should matchPattern { case Some(_) => }
//    println(s"flatland tree: ${meo.get}")
//    meo.exists(_.includes(Leaf(Value("flatlander")))) shouldBe true
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
//    val x = mptt.index.keySet.toList
//    val xSorted = x.sorted
//
//    def check(a: (String, String)): Boolean = !testMpttLookup(mptt, a._1, a._2)
//    println((x zip xSorted))
//    val r = (x zip xSorted) find (check)
//    r shouldBe None
  }

  def testMpttLookup(mPTT: MPTT[String], x: String, y: String): Boolean = x.contains(y) == (x.compareTo(y)<1)
}
