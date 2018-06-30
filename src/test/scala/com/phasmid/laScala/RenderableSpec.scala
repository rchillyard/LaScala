/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

import com.phasmid.laScala.fp.RenderableFunctionException
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

  import RenderableInstances._

  it should "render String the hard way" in {
    import Renderable._
    render("Hello")() shouldBe "Hello"
  }
  it should "render String" in {
    import RenderableSyntax._
    "Hello".render shouldBe "Hello"
  }
  it should "render Int" in {
    import RenderableSyntax._
    1.render shouldBe "1"
  }
  it should "render simple scalar values like toString" in {
    Scalar("x").render(1) shouldBe "x"
  }
  it should "render Seq values with indentation" in {
    val seq = Seq(Scalar("x"), Scalar("y"), Scalar("z"))
    import RenderableSyntax._
    seq.render shouldBe "(\n  x,\n  y,\n  z\n)"
  }
  it should "render Iterable values with indentation" in {
    val iterable = Iterable(Scalar("x"), Scalar("y"), Scalar("z"))
    import RenderableSyntax._
    iterable.render shouldBe "(\n  x,\n  y,\n  z\n)"
  }
  it should "render List values with indentation" in {
    val list = List(Scalar("x"), Scalar("y"), Scalar("z"))
    import RenderableSyntax._
    list.render shouldBe "(\n  x,\n  y,\n  z\n)"
  }
  it should "render list of length one" in {
    val list = Seq(Scalar("x"))
    list.render() shouldBe "(x)"
  }
  //  it should "render list values with indentation (custom RenderableTraversable)" in {
  //    implicit def r(xs: Traversable[_]): Renderable = RenderableTraversable(xs, "[;]")
  //
  //    val list = Seq(Scalar("x"), Scalar("y"), Scalar("z"))
  //    list.render() shouldBe "[\n  x;\n  y;\n  z\n]"
  //  }
  //  it should "limit the rendering of elements to " + Renderable.MAX_ELEMENTS in {
  //    import Renderable.renderableTraversable
  //    val list: List[Int] = (Stream from 1 take 20).toList
  //    list.render() shouldBe "(\n  1,\n  2,\n  3,\n  4,\n  5,\n  6,\n  7,\n  8,\n  9,\n  10,\n  ...[10 more elements]\n)"
  //  }
  //  it should "render list values with a max" in {
  //    implicit def r(xs: Traversable[_]): Renderable = RenderableTraversable(xs, max = Some(3))
  //
  //    val list = Seq(Scalar("x"), Scalar("y"), Scalar("z"), Scalar(""))
  //    list.render() shouldBe "(\n  x,\n  y,\n  z,\n  ...[1 more elements]\n)"
  //  }
  it should "render list values with double indentation" in {
    val list = Seq(Seq(Scalar("x0"), Scalar("x1")), Seq(Scalar("y0"), Scalar("y1")), Seq(Scalar("z0"), Scalar("z1")))
    list.render() shouldBe "(\n  (\n    x0,\n    x1\n  ),\n  (\n    y0,\n    y1\n  ),\n  (\n    z0,\n    z1\n  )\n)"
  }
//  it should "render list values with double indentation (custom tab)" in {
//    val tabulator = Seq(" " * 1, " " * 2, " " * 3)
//    implicit val renderer = new Renderable[Scalar] {
//      def render(s: Scalar)(indent: Int): String = s.render(indent)
//
//      override def tab(indent: Int): String = tabulator(indent)
//    }
//    import RenderableSyntax._
//    1.render shouldBe "1"
//    val list = List(List(Scalar("x0"), Scalar("x1")), List(Scalar("y0"), Scalar("y1")), List(Scalar("z0"), Scalar("z1")))
//    import Renderable._
//    import RenderableInstances._
//    import RenderableSyntax.RenderableOpsList
//    list.render shouldBe "(\n  (\n   x0,\n   x1\n  ),\n  (\n   y0,\n   y1\n  ),\n  (\n   z0,\n   z1\n  )\n )"
//  }
  it should "render Some" in {
    val xo = Option(Scalar("x"))
    import RenderableSyntax._
    xo.render shouldBe "Some(x)"
  }
    it should "render None" in {
      import RenderableSyntax._
      val xo: Option[String] = Option(null)
      xo.render shouldBe "None"
    }
  it should "render Success" in {
    val xy = Try(Scalar("x"))
    import RenderableSyntax._
    xy.render shouldBe "Success(x)"
  }
    it should "render Failure" in {
      import RenderableSyntax._
      val xy = Try[String](throw RenderableFunctionException("test"))
      xy.render shouldBe "Failure(test)"
    }
  it should "render either values (left)" in {
    val e: Either[Scalar, Any] = Left(Scalar("x"))
    import RenderableSyntax._
    e.render shouldBe "Left(x)"
  }
  it should "render either values (right)" in {
    val e: Either[Any, Scalar] = Right(Scalar("x"))
    e.render() shouldBe "Right(x)"
  }
  ////  it should "render Tuple2" in {
  ////    val t: Product = "x" -> "y"
  ////    import Renderable.renderableProduct
  ////    t.render() shouldBe """Tuple2("x","y")"""
  ////  }
  //  it should "render case class as a Product" in {
  //    val p: Product = Z(1, Math.PI, Seq("Hello", "Goodbye"))
  //    import Renderable.renderableProduct
  //    p.render() shouldBe
  //      s"""Z(1,3.141592653589793,(
  //    "Hello",
  //    "Goodbye"
  //  ))"""
  //  }
  //  // NOTE: please note that this test will not work for 2.10 - it will throw an exception
  //  // TODO refactor so that this test is not run for 2.10
  //  ignore should "render case class as a Case Class" in {
  //    val z = Z(1, Math.PI, Seq("Hello", "Goodbye"))
  //    val r = RenderableCaseClass(z)
  //    println(r.render())
  //    r.render() shouldBe "Z(\n  x:1\n  y:3.141592653589793\n  z:(\n      \"Hello\",\n      \"Goodbye\"\n    )\n  )"
  //  }

  behavior of "MockContainer"

  it should "render correctly" in {
    val target = MockContainer(Seq(1,2,3))
    target.toString shouldBe "(\n  1,\n  2,\n  3\n)"
  }
}

case class MockContainer[A: Renderable](as: Seq[A]) {
  import RenderableSyntax._
  import RenderableInstances._
  override def toString: String = as.render
}

case class MockNumericContainer[A: Renderable: Numeric](as: Seq[A]) {
  import RenderableSyntax._
  import RenderableInstances._
  override def toString: String = as.render
  def total: A = as.sum
}