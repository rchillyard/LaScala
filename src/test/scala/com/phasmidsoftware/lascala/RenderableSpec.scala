/*
 * LaScala
 * Copyright (c) 2017, 2026. Phasmid Software
 */

package com.phasmidsoftware.lascala

import com.phasmidsoftware.lascala.fp.RenderableFunctionException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Try}

/**
  * Tests of the type class Renderable.
  *
  * The expected strings of the 1.x tests (now in SelfRenderableSpec) are kept, with Sym in place of Scalar.
  */
class RenderableSpec extends AnyFlatSpec with Matchers {

  import Renderable.given
  import RenderableSpec.*

  behavior of "Renderable"

  it should "render simple values like toString" in {
    Sym("x").render shouldBe "x"
    1.render shouldBe "1"
  }
  it should "render a String with quotes" in {
    "Hello".render shouldBe "\"Hello\""
  }
  it should "render list values with indentation" in {
    val list = Seq(Sym("x"), Sym("y"), Sym("z"))
    list.render shouldBe "(\n  x,\n  y,\n  z\n)"
  }
  it should "render list of length one" in {
    val list = Seq(Sym("x"))
    list.render shouldBe "(x)"
  }
  it should "render list values with indentation (custom bookends)" in {
    given Layout = Layout(bookends = "[;]")

    val list = Seq(Sym("x"), Sym("y"), Sym("z"))
    list.render shouldBe "[\n  x;\n  y;\n  z\n]"
  }
  it should "limit the rendering of elements to " + Layout.MAX_ELEMENTS in {
    val list: List[Int] = (LazyList from 1 take 20).toList
    list.render shouldBe "(\n  1,\n  2,\n  3,\n  4,\n  5,\n  6,\n  7,\n  8,\n  9,\n  10,\n  ...[10 more elements]\n)"
  }
  it should "render list values with a max" in {
    given Layout = Layout(maxElements = Some(3))

    val list = Seq(Sym("x"), Sym("y"), Sym("z"), Sym(""))
    list.render shouldBe "(\n  x,\n  y,\n  z,\n  ...[1 more elements]\n)"
  }
  it should "render list values with double indentation" in {
    val list = Seq(Seq(Sym("x0"), Sym("x1")), Seq(Sym("y0"), Sym("y1")), Seq(Sym("z0"), Sym("z1")))
    list.render shouldBe "(\n  (\n    x0,\n    x1\n  ),\n  (\n    y0,\n    y1\n  ),\n  (\n    z0,\n    z1\n  )\n)"
  }
  it should "render list values with double indentation (custom tab)" in {
    val tabulator = Seq(" " * 1, " " * 2, " " * 3)

    given Layout = Layout(tab = tabulator)

    val list = Seq(Seq(Sym("x0"), Sym("x1")), Seq(Sym("y0"), Sym("y1")), Seq(Sym("z0"), Sym("z1")))
    list.render shouldBe "(\n  (\n   x0,\n   x1\n  ),\n  (\n   y0,\n   y1\n  ),\n  (\n   z0,\n   z1\n  )\n )"
  }
  it should "use the default Layout outside the block that declares another" in {
    val list = Seq(Sym("x"), Sym("y"))
    locally {
      given Layout = Layout(tab = Layout.uniform(4))

      list.render shouldBe "(\n    x,\n    y\n)"
    }
    list.render shouldBe "(\n  x,\n  y\n)"
  }
  it should "render Some" in {
    val xo = Option(Sym("x"))
    xo.render shouldBe "Some(x)"
  }
  it should "render None" in {
    val xo: Option[String] = Option(null)
    xo.render shouldBe "None"
  }
  it should "render Success" in {
    val xy = Try(Sym("x"))
    xy.render shouldBe "Success(x)"
  }
  it should "render Failure" in {
    val xy: Try[Sym] = Failure(RenderableFunctionException("test"))
    xy.render shouldBe "Failure(test)"
  }
  it should "render either values (left)" in {
    val e: Either[Sym, Int] = Left(Sym("x"))
    e.render shouldBe "Left(x)"
  }
  it should "render either values (right)" in {
    val e: Either[Int, Sym] = Right(Sym("x"))
    e.render shouldBe "Right(x)"
  }
  it should "render Tuple2" in {
    val t = "x" -> "y"
    t.render shouldBe """Tuple2("x","y")"""
  }
  it should "render a tuple containing a collection" in {
    val t = (1, Math.PI, Seq("Hello", "Goodbye"))
    t.render shouldBe
      s"""Tuple3(1,3.141592653589793,(
    "Hello",
    "Goodbye"
  ))"""
  }
  it should "render a Map" in {
    Map("x" -> 1, "y" -> 2).render shouldBe "(\n  \"x\"->1,\n  \"y\"->2\n)"
  }
  it should "render a collection of a different type, using the same instance" in {
    Vector(1, 2).render shouldBe "(\n  1,\n  2\n)"
    Set(1).render shouldBe "(1)"
  }

  behavior of "derived Renderable"

  it should "render a case class with one field per line" in {
    val z = Z(1, Math.PI, Seq("Hello", "Goodbye"))
    z.render shouldBe "Z(\n  x:1,\n  y:3.141592653589793,\n  z:(\n    \"Hello\",\n    \"Goodbye\"\n  )\n)"
  }
  it should "render a nested case class" in {
    Line(Point(1, 2), Point(3, 4)).render shouldBe "Line(\n  from:Point(\n    x:1,\n    y:2\n  ),\n  to:Point(\n    x:3,\n    y:4\n  )\n)"
  }
  it should "render a recursive enum" in {
    val tree = Tree.Node(Tree.Leaf, 1, Tree.Node(Tree.Leaf, 2, Tree.Leaf))
    tree.render shouldBe "Node(\n  left:Leaf,\n  value:1,\n  right:Node(\n    left:Leaf,\n    value:2,\n    right:Leaf\n  )\n)"
  }
  it should "render the cases of a sealed trait" in {
    val shapes: List[Shape] = List(Circle(1.0), Dot)
    shapes.render shouldBe "(\n  Circle(\n    radius:1.0\n  ),\n  Dot\n)"
  }
  it should "use a case's own instance when it has one" in {
    val coins: List[Coin] = List(Heads, Tails)
    coins.render shouldBe "(\n  H,\n  T\n)"
  }
  it should "not compile for a type with no instance" in {
    "NoInstance(1).render" shouldNot compile
    "Renderable.fromToString[NoInstance].render(NoInstance(1), 0)" should compile
  }
}

/**
  * Types used by RenderableSpec (in an object so that their names do not clash with those of other specs).
  */
object RenderableSpec {
  case class Sym(s: String)

  object Sym {
    given Renderable[Sym] = new Renderable[Sym] {
      def render(sym: Sym, indent: Int)(using Layout): String = sym.s
    }
  }

  case class Z(x: Int, y: Double, z: Seq[String]) derives Renderable

  case class Point(x: Int, y: Int) derives Renderable

  case class Line(from: Point, to: Point) derives Renderable

  enum Tree derives Renderable {
    case Leaf
    case Node(left: Tree, value: Int, right: Tree)
  }

  sealed trait Shape derives Renderable

  case class Circle(radius: Double) extends Shape

  case object Dot extends Shape

  sealed trait Coin derives Renderable

  case object Heads extends Coin {
    given Renderable[Heads.type] = new Renderable[Heads.type] {
      def render(h: Heads.type, indent: Int)(using Layout): String = "H"
    }
  }

  case object Tails extends Coin {
    given Renderable[Tails.type] = new Renderable[Tails.type] {
      def render(t: Tails.type, indent: Int)(using Layout): String = "T"
    }
  }

  case class NoInstance(x: Int)
}
