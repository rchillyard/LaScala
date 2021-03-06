package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.{FP, Spy}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Try

/**
  * Created by scalaprof on 10/19/16.
  */
class MPTTFunctionalSpec extends FlatSpec with Matchers {

  implicit object StringStringValueOps$ extends StringValueOps[String] {
    def getParentKey(a: String): Option[String] = Some(a.substring(0, a.length - 1))

    def createValueFromKey(k: String, vo: => Option[String]): Option[String] = Some(k)
  }

  implicit object StringIntValueOps$ extends StringValueOps[Int] {
    def getParentKey(a: Int): Option[String] = Some(a./(10).toString)

    def createValueFromKey(k: String, vo: => Option[Int]): Option[Int] = Try(k.toInt).toOption
  }

  behavior of "real-life UnvaluedBinaryTree"
  // FIXME resuscitate this test
  ignore should "build correctly" in {
    val uo = Option(getClass.getResource("flatland.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map (_.openStream)
    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
    import scala.language.postfixOps
    val z: Seq[String] = wso match {
      case Some(ws) => ws map (_.toLowerCase) filterNot (_.isEmpty) distinct
      case _ => Seq[String]()
    }
    import UnvaluedBinaryTree._
    trait OrderingValueString extends Ordering[String] {
      def compare(x: String, y: String): Int = x.compareTo(y)
    }
    val to = Spy.noSpy(Tree.populateOrderedTree(z))
    to match {
      case Some(tree) =>
        val mptt = MPTT(Tree.createIndexedTree(tree.asInstanceOf[UnvaluedBinaryTree[String]]))
        mptt.index.size shouldBe 176

        println(mptt)

        println(mptt.index.keySet)

        val nodes = tree.nodeIterator().filter(_.isLeaf).toList
        println(nodes)
        val flatland = nodes.find(x => FP.contains(x.get, "flatland"))
        flatland should matchPattern { case Some(_) => }
        flatland match {
          case Some(node) => node.includesValue("flatland") shouldBe true
          case _ => fail("logic")
        }
      case None => fail("could not populate a tree")
    }

  }
}
