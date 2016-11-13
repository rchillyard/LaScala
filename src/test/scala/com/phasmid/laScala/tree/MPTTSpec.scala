package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class MPTTSpec extends FlatSpec with Matchers {

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

  behavior of "real-life BinaryTree"
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
    val tree = TreeLike.populateTree(z)
    val indexedTree = Tree.createIndexedTree(tree.asInstanceOf[BinaryTree[String]], 0)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[String]])
    mptt.index.size shouldBe 177
    println(mptt)

  }

}
