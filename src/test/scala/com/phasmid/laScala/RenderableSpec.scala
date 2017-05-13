/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

import com.phasmid.laScala.values.Scalar
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.language.implicitConversions
import scala.util.Try

/**
  * See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  *
  * @author scalaprof
  */
class RenderableSpec extends FlatSpec with Matchers with Inside {
  behavior of "Renderable"
  it should "render simple values like toString" in {
    Scalar("x").render(1) shouldBe "x"
  }
  it should "render list values with indentation" in {
    val list = Seq(Scalar("x"), Scalar("y"), Scalar("z"))
    list.render() shouldBe "(\n  x,\n  y,\n  z\n)"
  }
  it should "render list values with indentation (custom RenderableTraversable)" in {
    implicit def r(xs: Traversable[_]): Renderable = RenderableTraversable(xs, "[;]")
    val list = Seq(Scalar("x"), Scalar("y"), Scalar("z"))
    list.render() shouldBe "[\n  x;\n  y;\n  z\n]"
  }
  it should "render list values with double indentation" in {
    val list = Seq(Seq(Scalar("x0"), Scalar("x1")), Seq(Scalar("y0"), Scalar("y1")), Seq(Scalar("z0"), Scalar("z1")))
    list.render() shouldBe "(\n  (\n    x0,\n    x1\n  ),\n  (\n    y0,\n    y1\n  ),\n  (\n    z0,\n    z1\n  )\n)"
  }
  it should "render list values with double indentation (custom tab)" in {
    val tabulator = Seq(" "*1," "*2," "*3)
    implicit def tab(indent: Int): Prefix = Prefix(tabulator(indent))
    val list = Seq(Seq(Scalar("x0"), Scalar("x1")), Seq(Scalar("y0"), Scalar("y1")), Seq(Scalar("z0"), Scalar("z1")))
    list.render() shouldBe "(\n  (\n   x0,\n   x1\n  ),\n  (\n   y0,\n   y1\n  ),\n  (\n   z0,\n   z1\n  )\n )"
  }
  it should "render option values" in {
    val xo = Option(Scalar("x"))
    xo.render() shouldBe "Some(x)"
  }
  it should "render try values" in {
    val xy = Try(Scalar("x"))
    xy.render() shouldBe "Success(x)"
  }
  it should "render either values" in {
    val e: Either[Scalar, Any] = Left(Scalar("x"))
    e.render() shouldBe "Left(x)"
  }
//  it should "render tuples" in {
//    val t: Product = "x" -> "y"
//    import Renderable.renderableProduct
//    t.render() shouldBe """("x","y")"""
//  }
}
