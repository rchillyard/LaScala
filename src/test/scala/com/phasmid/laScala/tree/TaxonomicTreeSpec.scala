/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.Spy
import com.phasmid.laScala.parser.{Header, TupleStream}
import com.phasmid.laScala.values.Scalar
import com.phasmid.laScala.{Prefix, OldRenderable, OldRenderableTraversable}
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.Logger

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Originally created by scalaprof on 10/19/16.
  *
  * A Taxon (an element within a Taxonomy)
  *
  * @param po   an alternative name, not normally used for matching, only for rendering.
  * @param taxa the ancestry (including this) of this taxon (two elements at least are required: self and parent, except top level).
  */
case class Taxon[P, Q](po: Option[P], taxa: Seq[(String, Q)])(implicit q2p: Q => P) extends OldRenderable {

  implicit def renderableTraversable(xs: Traversable[_]): OldRenderable = OldRenderableTraversable(xs, "[-]", linear = true)

  def render(indent: Int)(implicit tab: (Int) => Prefix): String = po.map(_ + ":").getOrElse("") + structuralKey

  private def values: Seq[Q] = for ((_, q) <- taxa) yield q

  def key: String = (for (p <- po) yield p.toString).getOrElse("undefined")

  /**
    * Method to form the "structural" key for this Taxon.
    * This is used when building a tree from many Taxa, but not when doing a lookup of a Taxon.
    * We return a rendering of the values of the taxa (we ignore po), dropping the first and last characters,
    * as what we want is enclosed in "[]"
    *
    * @return the structural key for this taxon
    */
  def structuralKey: String = values.toList.render().drop(1).dropRight(1)

  /**
    * We return the key of the parent of this taxon.
    *
    * @return the key for the parent of this taxon.
    */
  def parentKey: String =
    if (taxa.isEmpty)
      throw EmptyTaxonomy
    else parentTaxon.structuralKey

  def parentTaxon: Taxon[P, Q] = {
    val po = taxa.toList.reverse match {
      case _ :: g :: _ => Some(q2p(g._2))
      case _ => None
    }
    Taxon(po, taxa.init)
  }

  override def toString: String = render()
}

object Taxon {

  import Taxonomy._
  def apply(xWs: Seq[(String, Scalar)]): MockTaxon = Taxon(Some(xWs.head._2.source.toString), xWs.tail)
}

/**
  * Created by scalaprof on 1/2/17.
  */
object Taxonomy {
  implicit def conv(s: Scalar): String = s.source.toString

  type MockTaxon = Taxon[String, Scalar]

  trait TaxonValueOps[P, Q] extends StringValueOps[Taxon[P, Q]] {

    import Spy._

    def getParentKey(v: Taxon[P, Q]): Option[String] = Try(v.parentKey) match {
      case Success(k) => Some(k)
      case Failure(EmptyTaxonomy) => Spy.log(s"taxon $v has empty taxa"); None
      case Failure(x) => x.printStackTrace(System.err); None
    }

    /**
      * If the key is empty, then we must create a root node, which we do arbitrarily using None as the value,
      * and an empty Seq for the keys.
      *
      * @param k  the key for the parent we must create
      * @param vo a default, optional, value to be applied in the case that no other value can be reasonably be created
      * @return a value for the parent node, wrapped in Try
      */
    def createValueFromKey(k: String, vo: => Option[Taxon[P, Q]]): Option[Taxon[P, Q]]

    override def getKeyAsParent(v: Taxon[P, Q]): String = v.structuralKey

    override def getKeyFromValue(v: Taxon[P, Q]): String = v.key
  }

  implicit object MockTaxonValueOps extends TaxonValueOps[String, Scalar] {
    def createValueFromKey(k: String, vo: => Option[Taxon[String, Scalar]]): Option[Taxon[String, Scalar]] = if (k == "") Some(Taxon[String, Scalar](None, Seq[(String, Scalar)]())) else vo map (_.parentTaxon)
  }

  //  implicit object GeneralKVTreeBuilderTaxon extends GeneralKVTreeBuilder[String, MockTaxon]

  implicit object GeneralKVTreeBuilderTaxon extends GeneralKVTreeBuilderWithScaffolding[String, MockTaxon]

  def buildTaxonomy(ts: Stream[MockTaxon]): Try[Node[MockTaxon]] = {

    implicit val spyLogger: Logger = Spy.getLogger(getClass)

    def populateParentChildTree[V](values: Seq[V])(implicit treeBuilder: TreeBuilder[V], vo: ValueOps[String, V]): Try[Tree[V]] = {
      val ty = Spy.spy("root", Try(treeBuilder.buildTree(vo.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, V]]))

      @tailrec
      def inner(result: Try[StructuralTree[String, V]], values: List[V]): Try[StructuralTree[String, V]] = values match {
        case Nil => result
        case y :: z => inner(for (t <- result; u = t :+ y) yield u, z)
      }

      inner(ty, values.toList)
    }

    populateParentChildTree[MockTaxon](ts)
  }
}

case object EmptyTaxonomy extends Exception("cannot get parentKey for taxon with empty taxa")

/**
  * Created by scalaprof on 10/19/16.
  */
class TaxonomicTreeSpec extends FlatSpec with Matchers {

  import Taxonomy._

  private val taxonA = Taxon(Some("a"), Seq("name" -> Scalar("A")))
  private val taxonA1 = Taxon(Some("a1"), Seq("parent" -> Scalar("A"), "name" -> Scalar("A1")))

  behavior of "Taxon nodes"
  it should "render correctly" in {
    taxonA.render() shouldBe "a:A"
    taxonA1.render() shouldBe "a1:A-A1"
  }

  behavior of "TaxonValueOps"
  it should "implement getKeyFromValue properly" in {
    MockTaxonValueOps.getKeyFromValue(taxonA) shouldBe "a"
  }
  it should "implement getKeyAsParent properly" in {
    import Taxonomy._
    val valueOps = implicitly[ValueOps[String, MockTaxon]]
    valueOps.getKeyAsParent(taxonA) shouldBe "A"
  }
  it should "implement getParentKey properly" in {
    import Taxonomy._
    val valueOps = implicitly[ValueOps[String, MockTaxon]]
    valueOps.getParentKey(taxonA) shouldBe Some("")
    valueOps.getParentKey(taxonA1) shouldBe Some("A")
  }
  it should "implement createValueFromKey properly" in {
    import Taxonomy._
    val valueOps = implicitly[ValueOps[String, MockTaxon]]
    // NOTE: this looks odd but it's correct for a taxonomy
    valueOps.createValueFromKey("[A]", None) shouldBe None
  }

  behavior of "GeneralKVTreeBuilderTaxon"
  it should "implement buildLeaf properly" in {
    val leaf: Node[MockTaxon] = GeneralKVTreeBuilderTaxon.buildLeaf(taxonA)
    leaf.get should matchPattern { case Some(Taxon(Some("a"), _)) => }
    leaf.depth shouldBe 1
    leaf.size shouldBe 1
    leaf.isLeaf shouldBe true
    leaf.includes(leaf) shouldBe true
    leaf.includesValue(Taxon[String, Scalar](None, Seq())) shouldBe false
    leaf.includesValue(taxonA) shouldBe true
  }
  it should "implement nodesAlike properly" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[MockTaxon]]
    val leaf = treeBuilder.buildLeaf(taxonA)
    treeBuilder.nodesAlike(leaf, leaf) shouldBe true
  }
  it should "get the correct tree root" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[MockTaxon]]
    val valueOps = implicitly[ValueOps[String, MockTaxon]]
    val xy = Try(treeBuilder.buildTree(valueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, MockTaxon]])
    val ty = for (xt <- xy) yield for (t <- xt.get) yield (t.po, t.taxa)
    ty should matchPattern { case Success(Some((None, _))) => }
  }
  it should "implement addNode" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[MockTaxon]]
    val a = "a"
    val _a = "A"
    val p = "parent"
    val xy = Try(treeBuilder.buildTree(MockTaxonValueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, MockTaxon]])
    xy should matchPattern { case Success(GeneralKVTreeWithScaffolding(Some(Taxon(None, _)), List())) => }
    val ty = for (xt <- xy) yield for (t <- xt.get) yield (t.po, t.taxa)
    ty should matchPattern { case Success(Some((None, _))) => }
    // NOTE: addNode is protected so, for now at least, we use :+ as a surrogate
    val ny = for (x <- xy) yield x :+ treeBuilder.buildLeaf(Taxon(Some(a), Seq(p -> _a)))
    ny should matchPattern { case Success(GeneralKVTreeWithScaffolding(Some(Taxon(None, _)), List(Leaf(Taxon(Some(_), _))))) => }
    (for (n <- ny) yield n.children) should matchPattern { case Success(List(Leaf(Taxon(Some(_), _)))) => }
  }
  it should "implement :+" in {
    import Taxonomy._
    val treeBuilder = implicitly[TreeBuilder[MockTaxon]]
    val a = "a"
    val _a = Scalar("A")
    val p = "parent"
    val xy: Try[KVTree[String, MockTaxon]] = Try(treeBuilder.buildTree(MockTaxonValueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, MockTaxon]])
    xy should matchPattern { case Success(GeneralKVTreeWithScaffolding(Some(Taxon(None, _)), List())) => }
    val ty: Try[Option[(Option[String], Seq[(String, Scalar)])]] = for (xt <- xy) yield for (t <- xt.get) yield (t.po, t.taxa)
    ty should matchPattern { case Success(Some((None, _))) => }
    val taxonA: Taxon[String, Scalar] = Taxon(Some(a), Seq(p -> _a))
    val n1y = for (x <- xy) yield x :+ taxonA
    n1y should matchPattern { case Success(GeneralKVTreeWithScaffolding(Some(Taxon(None, _)), List(Leaf(Taxon(Some(_), _))))) => }
    val n2y = for (n1 <- n1y) yield n1 :+ taxonA1
    n2y should matchPattern { case Success(GeneralKVTreeWithScaffolding(Some(Taxon(None, _)), List(GeneralKVTreeWithScaffolding(Some(`taxonA`), List(Leaf(`taxonA1`)))))) => }
    n2y.get.render() shouldBe "--(a:A--(a1:A-A1))"
  }

  it should "implement :+ as in leaf-only taxonomy" in {
    import Taxonomy._
    implicit object MockTaxonValueOps extends TaxonValueOps[String, Scalar] {
      def createValueFromKey(k: String, vo: => Option[Taxon[String, Scalar]]): Option[Taxon[String, Scalar]] = if (k == "") Some(Taxon[String, Scalar](None, Seq[(String, Scalar)]())) else vo map (_.parentTaxon)
    }
    implicit object GeneralKVTreeBuilderTaxon extends GeneralKVTreeBuilder[String, MockTaxon]
    val t1: KVTree[String, MockTaxon] = GeneralKVTreeBuilderTaxon.buildTree(MockTaxonValueOps.createValueFromKey("", None), Seq()).asInstanceOf[KVTree[String, MockTaxon]]
    t1.depth shouldBe 1
    t1.size shouldBe 1
    val xWm: Map[String, Scalar] = Map("name" -> Scalar("vertebrates"), "kingdom" -> Scalar("animalia"), "phylum" -> Scalar("chordata"))
    val x: MockTaxon = Taxon(xWm.toSeq)
    val t2 = t1 :+ x
    t2.depth shouldBe 3
    t2.size shouldBe 3
    val animals = t2.children.head
    animals.get should matchPattern { case Some(_) => }
    animals.get.get should matchPattern { case _: Taxon[_, _] => }
    animals.get.get.po should matchPattern { case Some(_) => }
    animals.get.get.po.get shouldBe "animalia"
    animals.get.get.structuralKey shouldBe "animalia"
    val vertebrates = animals.asInstanceOf[Tree[MockTaxon]].children.head
    vertebrates.get should matchPattern { case Some(_) => }
    vertebrates.get.get should matchPattern { case _: Taxon[_, _] => }
    vertebrates.get.get.po should matchPattern { case Some(_) => }
    vertebrates.get.get.po.get shouldBe "vertebrates"
    vertebrates.get.get.structuralKey shouldBe "animalia-chordata"
  }

  behavior of "buildTaxonomy"
  it should "work with complete taxonomy" in {
    GeneralKVTreeBuilderTaxon.scaffolding.clear()
    val ps = TupleStream[Product](getClass.getResource("taxonomy.txt"), Header(List(), allowPartial = true))
    val tsy = for (xWms <- ps.asMaps) yield for (xWm <- xWms) yield Taxon(xWm.toSeq)
    val yy: Try[Node[MockTaxon]] = for (ts <- tsy; z <- Taxonomy.buildTaxonomy(ts)) yield z
    GeneralKVTreeBuilderTaxon.scaffolding.size shouldBe 16
    val zy: Try[IndexedNode[MockTaxon]] = for (y <- yy) yield Tree.createIndexedTree(y)
    zy should matchPattern { case Success(_) => }
    val tree = zy.get
    println(tree.render())
    tree.depth shouldBe 9
    // TODO Figure out why the following no longer works...
    //    val mptt = MPTT(tree)
    //    val size1 = mptt.index.size
    //    size1 shouldBe 16
    //    println(mptt.render())
  }

  it should "work with leaves-only taxonomy" in {
    GeneralKVTreeBuilderTaxon.scaffolding.clear()
    val ps = TupleStream[Product](getClass.getResource("taxonomy-leaves.txt"), Header(List(), allowPartial = true))
    val tsy = for (xWms <- ps.asMaps) yield for (xWm <- xWms) yield Taxon(xWm.toSeq)
    val yy: Try[Node[MockTaxon]] = for (ts <- tsy; z <- Taxonomy.buildTaxonomy(ts)) yield z
    GeneralKVTreeBuilderTaxon.scaffolding.size shouldBe 16
    val zy: Try[IndexedNode[MockTaxon]] = for (y <- yy) yield Tree.createIndexedTree(y)
    zy should matchPattern { case Success(_) => }
    val tree = zy.get
    println(tree.render())
    tree.depth shouldBe 9
    // TODO Figure out why the following no longer works...
    //    val mptt = MPTT(tree)
    //    val size1 = mptt.index.size
    //    size1 shouldBe 16
    //    println(mptt.render())
  }
}
