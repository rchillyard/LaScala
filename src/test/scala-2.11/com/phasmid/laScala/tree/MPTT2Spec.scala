/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

/**
  * The test contained here does not compile in 2.10 (in fact it crashes the compiler)
  * Created by scalaprof on 10/19/16.
  */
class MPTT2Spec extends FlatSpec with Matchers {

  case class MyInt(x: Int, b: Boolean)

  behavior of "MPTT"
  it should "support containsConditions correctly" in {
    implicit object StringMyIntValueOps$ extends StringValueOps[MyInt] {
      def getParentKey(a: MyInt): Option[String] = Some(a.x./(10).toString)

      override def getKeyAsParent(v: MyInt): String = v.x.toString

      override def getKeyFromValue(v: MyInt): String = v.x.toString

      def createValueFromKey(k: String, vo: => Option[MyInt]): Option[MyInt] = Try(MyInt(k.toInt, b = true)).toOption
    }
    //noinspection NameBooleanParameters
    val tree: GeneralTree[MyInt] = GeneralTree(MyInt(0, false), Seq(GeneralTree(MyInt(1, true), Seq(Leaf(MyInt(11, true)), Leaf(MyInt(12, true)), GeneralTree(MyInt(2, false), Seq(Leaf(MyInt(21, true)), Leaf(MyInt(22, true))))))))
    val indexedTree = Tree.createIndexedTree(tree)
    val mptt = MPTT(indexedTree.asInstanceOf[IndexedNode[MyInt]])

    def check(x: MyInt, y: MyInt): Boolean = x.b != y.b

    def checkF(x: MyInt, y: MyInt): Option[Boolean] = Some(x.b != y.b)

    mptt.containsConditional("0", "0")(check) should matchPattern { case None => }
    mptt.containsConditional("0", "1")(check) should matchPattern { case Some(true) => }
    mptt.containsConditional("0", "11")(check) should matchPattern { case Some(true) => }
    mptt.containsConditionalF("0", "0")(checkF) should matchPattern { case None => }
    mptt.containsConditionalF("0", "1")(checkF) should matchPattern { case Some(true) => }
    mptt.containsConditionalF("0", "11")(checkF) should matchPattern { case Some(true) => }
  }

  def testMpttLookup(mPTT: MPTT[String], x: String, y: String): Boolean = x.contains(y) == (x.compareTo(y) < 1)
}
