package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

/**
  * Created by scalaprof on 10/19/16.
  */
//noinspection NameBooleanParameters
class MPTTSpec extends FlatSpec with Matchers {

  implicit object MPTTStringStringValueOps extends StringValueOps[String] {
    def getParentKey(a: String): Option[String] = Some(a.substring(0, a.length - 1))

    def createValueFromKey(k: String, vo: => Option[String]): Option[String] = Some(k)
  }

  implicit object MPTTStringIntValueOps extends StringValueOps[Int] {
    def getParentKey(a: Int): Option[String] = Some(a./(10).toString)

    def createValueFromKey(k: String, vo: => Option[Int]): Option[Int] = Try(k.toInt).toOption
  }

  behavior of "MPTTEntry.contains"
  it should "be true for (0,3).contains((1,2)" in {
    val m1 = MPTTEntry("A")(0, 3)
    val m2 = MPTTEntry("Aardvark")(1, 2)
    m1.contains(m2) shouldBe true
  }
  it should "be true for (0,1).contains((0,1)" in {
    val m1 = MPTTEntry("A")(0, 1)
    m1.contains(m1) shouldBe true
  }
  it should "be false for (1,2).contains((0,3)" in {
    val m1 = MPTTEntry("A")(0, 3)
    val m2 = MPTTEntry("Aardvark")(1, 2)
    m2.contains(m1) shouldBe false
  }

  behavior of "MPTTEntry.containsAny"
  it should "only look at Aardvark" in {
    val m1 = MPTTEntry("A")(0, 3)
    val m2 = MPTTEntry("Aardvark")(1, 2)
    val m3 = MPTTEntry("The Story of B")(4, 5)
    // NOTE: that we should only see Aardvark because is is included and so we don't need to look at the Story of B
    m1.containsAny(Seq(Some(m2), Some(m3)).view.map(x => {
      println(x); x
    }).toIterator) shouldBe true
  }
  it should "only look at both entries" in {
    val m1 = MPTTEntry("A")(0, 3)
    val m2 = MPTTEntry("Aardvark")(1, 2)
    val m3 = MPTTEntry("The Story of B")(4, 5)
    m1.containsAny(Seq(Some(m3), Some(m2)).view.map(x => {
      println(x); x
    }).toIterator) shouldBe true
  }

  behavior of "MPTTEntry.containedByAny"
  it should " should show A only" in {
    val m0 = MPTTEntry("A")(0, 3)
    val m1 = MPTTEntry("B")(4, 7)
    val m2 = MPTTEntry("Aardvark")(1, 2)
    val m3 = MPTTEntry("The Story of B")(5, 6)
    // NOTE: that we should only see A because is is included and so we don't need to look at the Story of B
    m2.containedByAny(Seq(Some(m0), Some(m1)).view.map(x => {
      println(x); x
    }).toIterator) shouldBe true
    m3.containedByAny(Seq(Some(m0), Some(m1)).view.toIterator) shouldBe true
  }
  it should " should show A and B" in {
    val m0 = MPTTEntry("A")(0, 3)
    val m1 = MPTTEntry("B")(4, 7)
    val m2 = MPTTEntry("Aardvark")(1, 2)
    val m4 = MPTTEntry("C food diet")(8, 9)
    m2.containedByAny(Seq(Some(m1), Some(m0)).view.map(x => {
      println(x); x
    }).toIterator) shouldBe true
    m4.containedByAny(Seq(Some(m1), Some(m0)).view.map(x => {
      println(x); x
    }).toIterator) shouldBe false
  }

  behavior of "real-life GeneralTree"
  it should "build correctly" in {
    val tree = GeneralTree(0, List(GeneralTree(1, List(Leaf(11), Leaf(12), Leaf(13), Leaf(14))), GeneralTree(2, List(Leaf(21), Leaf(22), Leaf(23))), Leaf(3), Leaf(4)))
    val indexedTree = Tree.createIndexedTree(tree)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[Int]])
    //    for (i <- mptt.index) println(s"${i._2}")
    mptt.contains("0", "0") should matchPattern { case Some(true) => }
    mptt.contains("0", "1") should matchPattern { case Some(true) => }
    mptt.contains("0", "11") should matchPattern { case Some(true) => }
    mptt.contains("0", "14") should matchPattern { case Some(true) => }
    mptt.contains("0", "2") should matchPattern { case Some(true) => }
    mptt.contains("0", "21") should matchPattern { case Some(true) => }
    mptt.contains("0", "23") should matchPattern { case Some(true) => }
    mptt.contains("0", "3") should matchPattern { case Some(true) => }
    mptt.contains("0", "4") should matchPattern { case Some(true) => }
    mptt.contains("1", "0") should matchPattern { case Some(false) => }
    mptt.contains("1", "1") should matchPattern { case Some(true) => }
    mptt.contains("1", "11") should matchPattern { case Some(true) => }
    mptt.contains("1", "14") should matchPattern { case Some(true) => }
    mptt.contains("1", "2") should matchPattern { case Some(false) => }
    mptt.contains("1", "21") should matchPattern { case Some(false) => }
    mptt.contains("1", "23") should matchPattern { case Some(false) => }
    mptt.contains("1", "3") should matchPattern { case Some(false) => }
    mptt.contains("1", "4") should matchPattern { case Some(false) => }
    mptt.contains("2", "0") should matchPattern { case Some(false) => }
    mptt.contains("2", "1") should matchPattern { case Some(false) => }
    mptt.contains("2", "11") should matchPattern { case Some(false) => }
    mptt.contains("2", "14") should matchPattern { case Some(false) => }
    mptt.contains("2", "2") should matchPattern { case Some(true) => }
    mptt.contains("2", "21") should matchPattern { case Some(true) => }
    mptt.contains("2", "23") should matchPattern { case Some(true) => }
    mptt.contains("2", "3") should matchPattern { case Some(false) => }
    mptt.contains("2", "4") should matchPattern { case Some(false) => }
    mptt.contains("3", "0") should matchPattern { case Some(false) => }
    mptt.contains("3", "1") should matchPattern { case Some(false) => }
    mptt.contains("3", "11") should matchPattern { case Some(false) => }
    mptt.contains("3", "14") should matchPattern { case Some(false) => }
    mptt.contains("3", "2") should matchPattern { case Some(false) => }
    mptt.contains("3", "21") should matchPattern { case Some(false) => }
    mptt.contains("3", "23") should matchPattern { case Some(false) => }
    mptt.contains("3", "3") should matchPattern { case Some(true) => }
    mptt.contains("3", "4") should matchPattern { case Some(false) => }
  }

  //    behavior of "real-life UnvaluedBinaryTree"
  //  it should "build correctly" in {
  //    val uo = Option(getClass.getResource("flatland.txt"))
  //    uo should matchPattern { case Some(_) => }
  //    val so = uo map {_.openStream}
  //    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
  //    import scala.language.postfixOps
  //    val z: Seq[String] = wso match {
  //      case Some(ws) => ws map {_.toLowerCase} filterNot {_.isEmpty} distinct
  //      case _ => Seq[String]()
  //    }
  //    import UnvaluedBinaryTree._
  //    trait OrderingValueString extends Ordering[String] {
  //      def compare(x: String, y: String): Int = x.compareTo(y)
  //    }
  //    val tree = Spy.noSpy(Tree.populateOrderedTree(z))
  //    val mptt = MPTT(Tree.createIndexedTree(tree.asInstanceOf[UnvaluedBinaryTree[String]]))
  //    mptt.index.size shouldBe 176
  //
  //    println(mptt)
  //
  //    println(mptt.index.keySet)
  //
  //    val nodes = tree.nodeIterator().filter(_.isLeaf).toList
  //    println(nodes)
  //    val flatland = nodes.find(x => FP.contains(x.get,"flatland"))
  //    flatland should matchPattern { case Some(_) => }
  //    flatland match {
  //      case Some(node) => node.includesValue("flatland") shouldBe true
  //      case _ => fail("logic")
  //    }

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
  //  }

  def testMpttLookup(mPTT: MPTT[String], x: String, y: String): Boolean = x.contains(y) == (x.compareTo(y) < 1)
}
