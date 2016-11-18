package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.HasStringKey
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.SortedSet
import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class MPTTSpec extends FlatSpec with Matchers {

  abstract class HasStringKeyString$ extends HasStringKey[String] {
    def getKey(x: String): String = x
  }

  implicit object HasStringKeyString$ extends HasStringKeyString$

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
    val tree = TreeLike.populateOrderedTree(z map(Value(_)))
    val mptt = MPTT(TreeLike.createIndexedTree(tree.asInstanceOf[UnvaluedBinaryTree[String]]).asInstanceOf[IndexedNode[String]])
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
