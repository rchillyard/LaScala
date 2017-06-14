/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.parser

import com.phasmid.laScala.fp.Spy
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class ProductStream2Spec extends FlatSpec with Matchers {
  behavior of "TupleStream"
  it should "read taxonomy.txt correctly" in {
    val ps = TupleStream[Product](getClass.getResource("taxonomy.txt"), Header(Seq(), allowPartial = true))
    //noinspection ScalaUnusedSymbol
    //    for (t <- ps.input) println(x)
    val header = ps.header
    //noinspection ScalaUnusedSymbol
    val tuples = ps.tuplesPartial(true)
    //    for (t <- tuples) println(x)
    import Spy._
    val tsy = for (xWms <- ps.asMaps) yield for (xWm <- xWms) yield Taxon(xWm)
    tsy.foreach(_.size shouldBe 15)
    Spy.spy("tsy", tsy)
    val wst = for (ts <- tsy) yield for (t <- ts) yield t.render()
    for (ws <- wst) {
      ws.head shouldBe "animals: animalia"
      ws(6) shouldBe "golden eagle: animalia--chordata--aves--accipitriformes--accipitridae--buteoninae--aquila--chyrsaetos"
    }
    for (ws <- wst; ws2 = ws.tail) {
      ws2.head shouldBe "vertebrates: animalia--chordata"
      ws2(6) shouldBe "Bonelli's eagle: animalia--chordata--aves--accipitriformes--accipitridae--buteoninae--aquila--fasciata"
    }
  }
}

//
//case class Taxon(name: Scalar, taxonomy: Map[String, Scalar]) extends Renderable {
//  def render(indent: Int)(implicit tab: (Int) => Prefix): String = name.render() + ": " + taxonomy.values.map(_.render()).mkString("--")
//}
//
//object Taxon {
//  def apply(m: Map[String, Scalar]): Taxon = Taxon(m.head._2, m.tail)
//}


