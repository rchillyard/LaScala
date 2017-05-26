package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

/**
  * Created by scalaprof on 10/19/16.
  */
//noinspection NameBooleanParameters
class MPTTSpec extends FlatSpec with Matchers {

  implicit object StringStringValueOps$ extends StringValueOps[String] {
    def getParentKey(a: String): Option[String] = Some(a.substring(0, a.length - 1))

    def createValueFromKey(k: String): Option[String] = Some(k)
  }

  implicit object StringIntValueOps$ extends StringValueOps[Int] {
    def getParentKey(a: Int): Option[String] = Some(a./(10).toString)

    def createValueFromKey(k: String): Option[Int] = Try(k.toInt).toOption
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

  behavior of "real-life GeneralTree"
  it should "build correctly" in {
    val tree = GeneralTree(0, Seq(GeneralTree(1, Seq(Leaf(11), Leaf(12), Leaf(13), Leaf(14))), GeneralTree(2, Seq(Leaf(21), Leaf(22), Leaf(23))), Leaf(3), Leaf(4)))
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
  it should "support containsConditions correctly" in {
    case class MyInt(x: Int, b: Boolean)
    implicit object StringMyIntValueOps$ extends StringValueOps[MyInt] {
      def getParentKey(a: MyInt): Option[String] = Some(a.x./(10).toString)

      override def getKeyAsParent(v: MyInt): String = v.x.toString

      override def getKeyFromValue(v: MyInt): String = v.x.toString

      def createValueFromKey(k: String): Option[MyInt] = Try(MyInt(k.toInt, true)).toOption
    }
    val tree: GeneralTree[MyInt] = GeneralTree(MyInt(0, false), Seq(GeneralTree(MyInt(1, true), Seq(Leaf(MyInt(11, true)), Leaf(MyInt(12, true)), GeneralTree(MyInt(2, false), Seq(Leaf(MyInt(21, true)), Leaf(MyInt(22, true))))))))
    val indexedTree = Tree.createIndexedTree(tree)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[MyInt]])
    //    for (i <- mptt.index) println(s"${i._2}")

    def check(x: MyInt, y: MyInt): Boolean = x.b != y.b

    def checkF(x: MyInt, y: MyInt): Option[Boolean] = Some(x.b != y.b)

    mptt.containsConditional("0", "0")(check) should matchPattern { case None => }
    mptt.containsConditional("0", "1")(check) should matchPattern { case Some(true) => }
    mptt.containsConditional("0", "11")(check) should matchPattern { case Some(true) => }
    mptt.containsConditionalF("0", "0")(checkF) should matchPattern { case None => }
    mptt.containsConditionalF("0", "1")(checkF) should matchPattern { case Some(true) => }
    mptt.containsConditionalF("0", "11")(checkF) should matchPattern { case Some(true) => }
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
