package com.phasmid.laScala.parser

import com.phasmid.laScala.fp.Spy
import org.scalatest.{FlatSpec, Matchers}
import com.phasmid.laScala.{Prefix, Renderable}
import com.phasmid.laScala.values.Scalar

/**
  * @author scalaprof
  */
class ProductStreamSpec extends FlatSpec with Matchers {
  """"Hello", "World!"""" should "be (String) stream via CSV" in {
    val stream = Stream(Tuple1("Hello"), Tuple1("World!"))
    val c = ConcreteProductStream[Tuple1[String]](Header(Seq("word")),stream)
    c.header shouldBe Header(List("word"),false)
    val wts = c.tuples
    wts.size shouldBe 2
    wts.head match {
      case Tuple1(s) => assert(s == "Hello")
    }
    wts.tail.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
  it should "support filter" in {
    val c = ConcreteProductStream[Tuple1[String]](Header(Seq("word")), Stream(Tuple1("Hello"), Tuple1("World!")))
    val d = c filter {_._1 startsWith "W"}
    val wts = d.tuples
    wts.size shouldBe 1
    wts.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
  it should "support map" in {
    val c = ConcreteProductStream[Tuple1[String]](Header(Seq("word")), Stream(Tuple1("Hello"), Tuple1("World!")))
    val d = c map { t => Tuple1(t._1.toLowerCase) }
    val wts = d.tuples
    wts.size shouldBe 2
    wts.head match {
      case Tuple1(s) => assert(s == "hello")
    }
  }
  //  it should "support flatMap" in {
  //    val c = ConcreteProductStream[Tuple1[String]](Seq("word"), Stream(Tuple1("Hello"), Tuple1("World!")))
  //    val d = c flatMap {_._1.iterator.toTuple}
  //    val wts = d.tuples
  //    wts.size shouldBe 1
  //    wts.head match {
  //      case Tuple1(s) => assert(s == "World!")
  //    }
  //  }

  behavior of "TupleStream"
  it should "read taxonomy.txt correctly" in {
    val ps = TupleStream[Product](getClass.getResource("taxonomy.txt"), Header(Seq(),true))
    for (t <- ps.input) println(t)
    val header = ps.header
    val tuples = ps.tuplesPartial(true)
    for (t <- tuples) println(t)
    import Spy._
    val tsy = for (xWms <- ps.asMaps) yield for(xWm <- xWms) yield Taxon(xWm)
    tsy.foreach(_.size shouldBe 15)
    Spy.spy("tsy",tsy)
    val wst = for (ts <- tsy) yield for(t <- ts) yield t.render()
    for (ws <- wst) {
      ws.head shouldBe "animals: animalia"
      ws(6) shouldBe "golden eagle: animalia--chordata--aves--accipitriformes--accipitridae--buteoninae--aquila--chyrsaetos"
    }
  }
}

case class Taxon(name: Scalar, taxonomy: Map[String,Scalar]) extends Renderable {
  def render(indent: Int)(implicit tab: (Int) => Prefix): String = name.render()+": "+taxonomy.values.map(_.render()).mkString("--")
}

object Taxon {
  def apply(m: Map[String,Scalar]): Taxon = Taxon(m.head._2, m.tail)
}


