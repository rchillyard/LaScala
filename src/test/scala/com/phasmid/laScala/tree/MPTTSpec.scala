package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.HasKey
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.SortedSet
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
  ignore should "build correctly" in {
    import GeneralTree._
    trait NodeTypeParentInt extends HasParent[Int] {
      def createParent(a: Int): Option[Node[Int]] = None
      def getParentKey(a: Int): Option[String] = Some(a./(10).toString)
    }
    val tree = GeneralTree(0, Nil) :+ 1 :+ 2 :+ 3 :+ 4 :+ 14
    tree shouldBe GeneralTree(0, Seq(Leaf(1), Leaf(2), Leaf(3), Leaf(4)))
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
    val tree = Tree.populateOrderedTree(z map(Value(_)))
    val mptt = MPTT(Tree.createIndexedTree(tree.asInstanceOf[UnvaluedBinaryTree[Value[String]]]).asInstanceOf[IndexedNode[Value[String]]])
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
