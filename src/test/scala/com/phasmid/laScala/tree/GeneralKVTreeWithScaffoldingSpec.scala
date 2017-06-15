/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.tree

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

/**
  * CONSIDER there's a lot of redundant tests here -- clean them out
  *
  * Created by scalaprof on 10/19/16.
  */
class GeneralKVTreeWithScaffoldingSpec extends FlatSpec with Matchers {

  implicit object IntStringValueOps$ extends StringValueOps[Int] {
    def getParentKey(t: Int): Option[String] = (for (i <- Try(t / 10); s = i.toString) yield s).toOption

    def createValueFromKey(k: String, vo: => Option[Int]): Option[Int] = Try(k.toInt).toOption
  }

  implicit object GeneralKVTreeWithScaffoldingTreeBuilderStringInt extends GeneralKVTreeBuilderWithScaffolding[String, Int]

  behavior of "GeneralKVTreeWithScaffolding"
  it should "work for find(Int)" in {
    GeneralKVTreeWithScaffoldingTreeBuilderStringInt.scaffolding.clear()
    val tree = GeneralKVTreeWithScaffoldingTreeBuilderStringInt.buildTree(Some(1), Seq(Leaf(2), Leaf(3)))
    tree.find(1) should matchPattern { case Some(GeneralKVTreeWithScaffolding(Some(1), Seq(Leaf(2), Leaf(3)))) => }
    tree.find(2) should matchPattern { case Some(Leaf(2)) => }
    tree.find(3) should matchPattern { case Some(Leaf(3)) => }
    tree.find(4) should matchPattern { case None => }
    tree.find(0) should matchPattern { case None => }
  }
  it should "work for findByParentKey" in {
    GeneralKVTreeWithScaffoldingTreeBuilderStringInt.scaffolding.clear()
    val tree1 = GeneralKVTreeWithScaffoldingTreeBuilderStringInt.buildTree(Some(0), Nil) :+ Leaf(1)
    GeneralKVTreeWithScaffoldingTreeBuilderStringInt.scaffolding.size shouldBe 2
    val tree2 = tree1 :+ Leaf(2)
    GeneralKVTreeWithScaffoldingTreeBuilderStringInt.scaffolding.size shouldBe 3
    tree2.findByParentKey("0") should matchPattern { case Some(GeneralKVTreeWithScaffolding(Some(0), Seq(Leaf(1), Leaf(2)))) => }
    val tree3 = tree2 :+ Leaf(21)
    GeneralKVTreeWithScaffoldingTreeBuilderStringInt.scaffolding.size shouldBe 4
    tree3.findByParentKey("0") should matchPattern { case Some(_) => }
    tree3.findByParentKey("2") should matchPattern { case Some(GeneralKVTreeWithScaffolding(Some(2), Seq(Leaf(21)))) => }
  }
}
