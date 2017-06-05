/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.Spy
import com.phasmid.laScala.parser.{Header, TupleStream}
import com.phasmid.laScala.tree.Taxonomy.{GeneralKVTreeBuilderTaxon, TaxonValueOps}
import com.phasmid.laScala.values.Scalar
import com.phasmid.laScala.{Prefix, Renderable, RenderableTraversable}
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}


/**
  * Created by scalaprof on 10/19/16.
  */
class TaxonomicTreeSpec extends FlatSpec with Matchers {

  private val taxonA = Taxon(Some("a"), Seq("name" -> "A"))
  private val taxonA1 = Taxon(Some("a1"), Seq("parent" -> "A", "name" -> "A1"))

  behavior of "Taxon nodes"
  it should "render correctly" in {
    taxonA.render() shouldBe "a:[A]"
    taxonA1.render() shouldBe "a1:[A-A1]"
  }

  behavior of "TaxonValueOps"
  it should "implement getKeyFromValue properly" in {
    TaxonValueOps.getKeyFromValue(taxonA) shouldBe "[A]"
  }
  it should "implement getKeyAsParent properly" in {
    import Taxonomy._
    val valueOps = implicitly[ValueOps[String, Taxon]]
    valueOps.getKeyAsParent(taxonA) shouldBe "[A]"
  }
  it should "implement getParentKey properly" in {
    import Taxonomy._
    val valueOps = implicitly[ValueOps[String, Taxon]]
    valueOps.getParentKey(taxonA) shouldBe Some("[]")
    valueOps.getParentKey(taxonA1) shouldBe Some("[A]")
  }
  it should "implement createValueFromKey properly" in {
    import Taxonomy._
    val valueOps = implicitly[ValueOps[String, Taxon]]
    // NOTE: this looks odd but it's correct for a taxonomy
    valueOps.createValueFromKey("[A]", None) shouldBe None
  }

  behavior of "GeneralKVTreeBuilderTaxon"
  it should "implement buildLeaf properly" in {
    val leaf: Node[Taxon] = GeneralKVTreeBuilderTaxon.buildLeaf(taxonA)
    leaf.get should matchPattern { case Some(Taxon(Some("a"), _)) => }
    leaf.depth shouldBe 1
    leaf.size shouldBe 1
    leaf.isLeaf shouldBe true
    leaf.includes(leaf) shouldBe true
    leaf.includesValue(Taxon(None, Seq())) shouldBe false
    leaf.includesValue(taxonA) shouldBe true
  }
  it should "implement nodesAlike properly" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[Taxon]]
    val leaf = treeBuilder.buildLeaf(taxonA)
    treeBuilder.nodesAlike(leaf, leaf) shouldBe true
  }
  it should "get the correct tree root" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[Taxon]]
    val valueOps = implicitly[ValueOps[String, Taxon]]
    val xy = Try(treeBuilder.buildTree(valueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, Taxon]])
    val ty = for (xt <- xy) yield for (t <- xt.get) yield (t.maybeName, t.taxa)
    ty should matchPattern { case Success(Some((None, _))) => }
  }
  it should "implement addNode" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[Taxon]]
    val a = "a"
    val _a = "A"
    val p = "parent"
    val xy = Try(treeBuilder.buildTree(TaxonValueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, Taxon]])
    xy should matchPattern { case Success(GeneralKVTree(Some(Taxon(None, _)), List())) => }
    val ty = for (xt <- xy) yield for (t <- xt.get) yield (t.maybeName, t.taxa)
    ty should matchPattern { case Success(Some((None, _))) => }
    // NOTE: addNode is protected so, for now at least, we use :+ as a surrogate
    val ny = for (x <- xy) yield x :+ treeBuilder.buildLeaf(Taxon(Some(a), Seq(p -> _a)))
    ny should matchPattern { case Success(GeneralKVTree(Some(Taxon(None, _)), List(Leaf(Taxon(Some(_), _))))) => }
    (for (n <- ny) yield n.children) should matchPattern { case Success(List(Leaf(Taxon(Some(_), _)))) => }
  }
  it should "implement :+" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[Taxon]]
    val a = "a"
    val _a = "A"
    val p = "parent"
    val xy = Try(treeBuilder.buildTree(TaxonValueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, Taxon]])
    xy should matchPattern { case Success(GeneralKVTree(Some(Taxon(None, _)), List())) => }
    val ty = for (xt <- xy) yield for (t <- xt.get) yield (t.maybeName, t.taxa)
    ty should matchPattern { case Success(Some((None, _))) => }
    val taxonA = Taxon(Some(a), Seq(p -> _a))
    val n1y = for (x <- xy) yield x :+ taxonA
    n1y should matchPattern { case Success(GeneralKVTree(Some(Taxon(None, _)), List(Leaf(Taxon(Some(_), _))))) => }
    val n2y = for (n1 <- n1y) yield n1 :+ taxonA1
    n2y should matchPattern { case Success(GeneralKVTree(Some(Taxon(None, _)), List(GeneralKVTree(Some(`taxonA`), List(Leaf(`taxonA1`)))))) => }
    n2y.get.render() shouldBe "[]\n  a:[A]\n    a1:[A-A1]"
  }
  it should "work" in {
    val ps = TupleStream[Product](getClass.getResource("taxonomy.txt"), Header(Seq(), allowPartial = true))
    val tsy = for (xWms <- ps.asMaps) yield for (xWm <- xWms) yield Taxon(xWm.toSeq)
    val yy: Try[Node[Taxon]] = for (ts <- tsy; z <- Taxonomy.buildTaxonomy(ts)) yield z
    val zy: Try[IndexedNode[Taxon]] = for (y <- yy) yield Tree.createIndexedTree(y)
    zy should matchPattern { case Success(_) => }
    val tree = zy.get
    println(tree.render())
    tree.depth shouldBe 9
    val mptt = MPTT(tree)
    val size1 = mptt.index.size
    size1 shouldBe 16
    println(mptt.render())
  }
}

/**
  * A Taxon (an element within a Taxonomy)
  *
  * @param maybeName an alternative name, not normally used for matching, only for rendering.
  * @param taxa      the ancestry (including this) of this taxon (two elements at least are required: self and parent, except top level).
  */
case class Taxon(maybeName: Option[String], taxa: Seq[(String, Scalar)]) extends Renderable {

  implicit def renderableTraversable(xs: Traversable[_]): Renderable = RenderableTraversable(xs, "[-]", linear = true)

  def render(indent: Int)(implicit tab: (Int) => Prefix): String = maybeName.map(_ + ":").getOrElse("") + key

  private def values: Seq[Scalar] = for ((_, q) <- taxa) yield q

  def key: String = values.toList.render() //  taxa.values.map(_.render()).mkString("--")

  def parentKey: String =
    if (taxa.isEmpty)
      throw EmptyTaxonomy
    else Taxon(None, taxa.init).key

  override def toString: String = render()
}

object Taxon {
  def apply(xWs: Seq[(String, Scalar)]): Taxon = Taxon(Some(xWs.head._2.source.toString), xWs.tail)
}

/**
  * Created by scalaprof on 1/2/17.
  */
object Taxonomy {

  trait TaxonValueOps extends StringValueOps[Taxon] {

    import Spy._

    def getParentKey(v: Taxon): Option[String] = Try(v.parentKey) match {
      case Success(k) => Some(k)
      case Failure(EmptyTaxonomy) => Spy.log(s"taxon $v has empty taxa"); None
      case Failure(x) => x.printStackTrace(System.err); None
    }

    /**
      * TODO sync this up with other implementation
      * If the key is empty, then we must create a root node, which we do arbitrarily using None as the value,
      * and an empty Seq for the keys.
      *
      * @param k  the key for the parent we must create
      * @param vo a default, optional, value to be applied in the case that no other value can be reasonably be created
      * @return a value for the parent node, wrapped in Try
      */
    def createValueFromKey(k: String, vo: => Option[Taxon]): Option[Taxon] = if (k == "") Some(Taxon(None, Seq())) else vo

    override def getKeyAsParent(v: Taxon): String = v.key

    override def getKeyFromValue(v: Taxon): String = v.key
  }

  implicit object TaxonValueOps extends TaxonValueOps

  implicit object GeneralKVTreeBuilderTaxon extends GeneralKVTreeBuilder[String, Taxon]

  def buildTaxonomy(ts: Stream[Taxon]): Try[Node[Taxon]] = {

    implicit val spyLogger = Spy.getLogger(getClass)

    def populateParentChildTree[V](values: Seq[V])(implicit treeBuilder: TreeBuilder[V], vo: ValueOps[String, V]): Try[Tree[V]] = {
      val ty = Spy.spy("root", Try(treeBuilder.buildTree(vo.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, V]]))

      @tailrec
      def inner(result: Try[StructuralTree[V]], values: List[V]): Try[StructuralTree[V]] = values match {
        case Nil => result
        case y :: z => inner(for (t <- result; u = t :+ y) yield u, z)
      }

      inner(ty, values.toList)
    }

    populateParentChildTree(ts)
  }
}

case object EmptyTaxonomy extends Exception("cannot get parentKey for taxon with empty taxa")