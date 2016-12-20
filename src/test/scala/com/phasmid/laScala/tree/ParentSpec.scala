package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by scalaprof on 10/19/16.
  */
class ParentSpec extends FlatSpec with Matchers {

//  implicit object IntStringKeyOps extends KeyOps[Int] {
//    type K = String
//    def getParentKey(v: Int): K = (v/10).toString
//    def getKey(v: Int): K = v.toString
//    def createValueFromKey(k: K): Int = k.toInt
//  }
//
//  implicit object StringStringKeyOps extends KeyOps[String] {
//    type K = String
//    def getParentKey(v: String): K = ??? // FIXME
//    def getKey(v: String): K = v.toString
//    def createValueFromKey(k: K): String = k
//  }

//  trait GeneralTreeBuilder[A] extends TreeBuilder[A] {
//    def buildTree(maybeValue: Option[A], children: Seq[Node[A]]): Tree[A] = GeneralTree(maybeValue.get,children)
//    def buildLeaf(a: A): Node[A] = Leaf(a)
//  }
////  implicit object GeneralTreeBuilderInt extends GeneralTreeBuilder[Int]
//  // TODO move this into GeneralTree (and get rid of this GeneralTreeBuilder)
//  implicit object GeneralTreeBuilderString extends GeneralTreeBuilder[String]

  trait GeneralNodeParent[A] extends NodeParent[A] {
    def isParent(parent: Node[A], child: Node[A]): Boolean = parent match {
      case Branch(_,ns) => ns.contains(child)
      case _ => false
    }
  }
//  implicit object GeneralNodeParentInt extends GeneralNodeParent[Int]
  implicit object GeneralNodeParentString extends GeneralNodeParent[String]

  trait NodeTypeParent[A] extends HasParent[String,A] {
    def getParent(tree: Tree[A], t: A): Option[Node[A]] = ??? // FIXME

    def createValueFromKey(k: String): A = null.asInstanceOf[A] // FIXME

    def createParent(a: A): Option[Node[A]] = None

    def getParentKey(a: A): Option[String] = None
  }
//  implicit object GeneralNodeTypeParentInt extends StringHasParent[Int]
// TODO move this into GeneralTree (and get rid of this StringHasParent)
//  implicit object GeneralNodeTypeParentString extends StringHasParent[String]

  behavior of "traverse"

  implicit object IntStringKeyOps extends KeyOps[String,Int] {
    def getKeyFromValue(v: Int): String = v.toString
    def getParentKey(v: Int): Option[String] = Some((v/10).toString)
  }
  implicit object StringStringKeyOps extends KeyOps[String,String] {
    def getKeyFromValue(v: String): String = v
    def getParentKey(v: String): Option[String] = Some(v.substring(0,v.length-1))
  }


  def f(t: Node[Int]): Option[Int] = t.get
  def g(ns: Seq[Int], io: Option[Int]): Seq[Int] = io match { case Some(i) => ns :+ i; case _ => ns}

  it should "work correctly for GenericTree" in {
    val tree = GeneralTree(1,Seq(Leaf(2),Leaf(3)))
    println(tree)
    Parent.traverse(f,g)(List[Node[Int]](tree),List[Int]()) shouldBe List(1,2,3)
  }
  it should "work correctly for UnvaluedBinaryTree" in {
    import UnvaluedBinaryTree._
    implicit object IntStringKeyOps extends KeyOps[String,Int] {
      def getKeyFromValue(v: Int): String = v.toString
      def getParentKey(v: Int): Option[String] = Some((v/10).toString)
    }
    implicit object StringIntHasParent extends HasParent[String,Int] {
      def getParent(tree: Tree[Int], t: Int): Option[Node[Int]] = ???
      def getParentKey(t: Int): Option[String] = ???
      def createValueFromKey(k: String): Int = ???
}

    val tree = UnvaluedBinaryTree(Leaf(1),UnvaluedBinaryTree(Leaf(2),Leaf(4)) :+ Leaf(3))
    println(tree)
    Parent.traverse(f,g)(List[Node[Int]](tree),List[Int]()) shouldBe List(1,2,3,4)
  }
  // FIXME this should be fixed -- but why is it a GeneralTree? Surely it should be a BinaryTree
  ignore should "work correctly for unsorted Flatland tree" in {
    val uo = Option(getClass.getResource("flatland.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map {_.openStream}
    val wso = for (s <- so) yield (for (l <- Source.fromInputStream(s).getLines; w <- l.split("""\W+""")) yield w).toList
    import scala.language.postfixOps
    val z: Seq[String] = wso match {
      case Some(ws) => ws map {_.toLowerCase} filterNot {_.isEmpty} distinct
      case _ => Seq[String]()
    }
    import GeneralTree._
    implicit object StringStringHasParent extends HasParent[String,String] {
      def getParent(tree: Tree[String], t: String): Option[Node[String]] = ???
      def getParentKey(t: String): Option[String] = ???
      def createValueFromKey(k: String): String = ???
}
    val tree = Tree.populateOrderedTree(z)
    println(tree)
    def f(t: Node[String]): Option[String] = t.get
    def g(ns: Seq[String], wo: Option[String]): Seq[String] = wo match { case Some(i) => ns :+ i; case _ => ns}
    // TODO recreate this test
//    Parent.traverse(f,g)(List[Node[String]](tree),List[String]()).take(12) shouldBe List("i", "flatland", "world", "because", "hexagons", "not", "years", "a", "but", "freely", "higher", "make")
  }
  val flatLandTree = "a{{about}above{{actually}ago{{alas}all{{and}another{{anything}appear{{are}as{{at}back{{be}because{{become}becoming{{been}below{{bringing}but{{by}call{{can}ceased{{circle}clearer{{condition}contrary{{correct}could{{country}countrymen{{dare}demonstrate{{described}distinguish{{down}drawing{{edge}edges{{exactly}except{{eye}far{{few}figure{{figures}find{{fixed}flatland{{flatlander}freely{{from}gradually{{happy}hard{{has}have{{hexagons}higher{{i}imagine{{impossible}in{{inhabitants}instead{{into}is{{it}its{{kind}last{{leaning}least{{like}line{{lines}live{{look}lower{{luminous}make{{middle}mind{{more}move{{moving}much{{my}nature{{necessity}nor{{not}nothing{{notion}now{{of}on{{once}one{{only}opened{{or}other{{our}oval{{over}p{{paper}penny{{pentagons}perceive{{place}placed{{places}power{{pretty}privileged{{readers}remaining{{rising}said{{say}see{{shadows}sheet{{should}sight{{sinking}so{{solid}space{{speedily}squares{{straight}such{{suppose}surface{{table}tables{{that}the{{their}them{{then}there{{things}this{{thus}to{{triangles}universe{{upon}us{{vast}very{{view}views{{visible}was{{we}were{{what}when{{which}who{{will}with{{without}world{{years}you{{your}yourself}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
}
