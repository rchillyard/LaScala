/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.{BooleanScalar, Scalar, StringScalar}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scala.util._

/**
  * Created by scalaprof on 10/19/16.
  */
class ClosureSpec extends FlatSpec with Matchers {

  val s2s: Scalar => Option[String] = { s => Option(s.render()) }
  val s2b: Scalar => Option[Boolean] = { s => s.asBoolean }

  behavior of "Closure"
  it should "apply for a simple closure" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name)
    val c = Closure(f, Left("Hello"))
    c.arity shouldBe 0
    c() shouldBe Success(true)
  }

  it should "implement bind" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name)
    val c1 = Closure[String,Boolean](f)
    c1.arity shouldBe 1
    val c2 = c1.bind(Left("Hello"))
    c2.arity shouldBe 0
    c2() shouldBe Success(true)
  }

  it should "throw exception where there is no parameter" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name)
    val c = Closure(f)
    c.arity shouldBe 1
    c() should matchPattern { case Failure(_) => }
    an[RenderableFunctionException] should be thrownBy c().get
  }

  behavior of "RenderableFunction.apply"

  it should "apply a 1-arity function" in {
    val name = "isHello"
    val arity = 1
    val f = RenderableFunction({ s: String => s == "Hello" }, name)
    f.w shouldBe FunctionString(name, arity)
    f.arity shouldBe arity
    f.func(Tuple1("Hello")) shouldBe true
    an[ClassCastException] should be thrownBy f.func("Hello")
  }

  it should "apply a 2-arity function" in {
    val name = "render"
    val arity = 2

    def render(s1: String, s2: String) = s1 + ":" + s2

    val f = RenderableFunction(render _, name)
    f.w shouldBe FunctionString(name, arity)
    f.arity shouldBe arity
    f.func(("Hello", "Goodbye")) shouldBe "Hello:Goodbye"
    an[ClassCastException] should be thrownBy f.func("Hello")
    an[ClassCastException] should be thrownBy f.func(Tuple1("Hello"))
  }

  behavior of "render"

  it should "give function string with tabs" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name)
    f.render(1) shouldBe s"  $name(a?)"
  }

  behavior of "FunctionString"

  it should "add n parameters" in {
    FunctionString("f",0).toString shouldBe "f"
    FunctionString("f",1).toString shouldBe "f(a?)"
    FunctionString("f",2).toString shouldBe "f(a?)(b?)"
    FunctionString("f",26).toString shouldBe "f(a?)(b?)(c?)(d?)(e?)(f?)(g?)(h?)(i?)(j?)(k?)(l?)(m?)(n?)(o?)(p?)(q?)(r?)(s?)(t?)(u?)(v?)(w?)(x?)(y?)(z?)"
  }

  it should "use custom parameter names" in {
    FunctionString.custom("f", List("x")).toString shouldBe "f(x?)"
    FunctionString.custom("f", List("x", "mean")).toString shouldBe "f(x?)(mean?)"
  }

  it should "calculate arity correctly" in {
    FunctionString("f",0).arity shouldBe 0
    FunctionString("f",1).arity shouldBe 1
    FunctionString("f",2).arity shouldBe 2
    FunctionString("f(1)",0).arity shouldBe 0
    FunctionString("f(true)",1).arity shouldBe 1
    FunctionString("f(3.1415927)",2).arity shouldBe 2
    FunctionString("""f("x")""",0).arity shouldBe 0
    FunctionString("""f("y")""",1).arity shouldBe 1
    FunctionString("""f("z")""",2).arity shouldBe 2
  }

  it should "calculate arity correctly for function" in {
    val f = FunctionString("f",1)
    f.arity shouldBe 1
    val g = FunctionString("g", 1)
    g.arity shouldBe 1
    val h = g.partiallyApplyFunction(f)
    println(s"h: $h")
    h.arity shouldBe 1
  }

  it should "partiallyApply correctly with index 0" in {
    FunctionString("f", 1).partiallyApply("x").toString shouldBe """f("x")"""
    FunctionString("f",2).partiallyApply("x").toString shouldBe """f("x")(b?)"""
    an[RenderableFunctionException] should be thrownBy FunctionString("f", 0).partiallyApply("x")
  }

  it should "partiallyApply correctly with index 1" in {
    FunctionString("f",2).partiallyApply("x",1).toString shouldBe """f(a?)("x")"""
    an[RenderableFunctionException] should be thrownBy FunctionString("f", 0).partiallyApply("x", 1)
    an[RenderableFunctionException] should be thrownBy FunctionString("f", 1).partiallyApply("x", 1)
  }

  it should "partiallyApplyFunction correctly" in {
    // TODO understand why this is a bit flaky
    val f = FunctionString("f",1)
    val g = FunctionString("g", 1)
    val h = g.partiallyApplyFunction(f)
    h.arity shouldBe 1
    h.toString shouldBe """g(f(a?))"""
  }

  behavior of "RenderableFunction.apply"

  it should "yield Hello with arity 1" in {
    def render(s: String) = s == "Hello"

    val f = RenderableFunction(render _, "render")
    val r = f(Tuple1("Hello"))
    r should matchPattern { case Success(true) => }
  }

  it should "yield Hello with arity 2" in {
    def render(s1: Scalar, s2: Scalar) = s1.render() + ":" + s2.render()

    val f = RenderableFunction(render _, "render")
    val r = f(Tuple2(Scalar("Hello"), Scalar(1)))
    r should matchPattern { case Success("Hello:1") => }
  }

  it should "yield Hello with arity 3" in {
    // XXX Not sure why we have to set this here, but we do.
    BooleanScalar.setDefaultFormat("%b")
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render()

    val f = RenderableFunction(render _, "render")
    val r = f(Tuple3(Scalar("Hello"), Scalar(1), Scalar(true)))
    val hello1true = "Hello:1:true"
    r should matchPattern { case Success(`hello1true`) => }
  }

  it should "handle function returning a Try" in {
    val map = Map("x" -> "X")

    def lookup(s: String): Try[String] = FP.optionToTry(map.get(s))

    val f = RenderableFunction(lookup _, "lookup")
    val r = f(Tuple1("x"))
    r should matchPattern { case Success("X") => }
  }

  behavior of "RenderableFunction.partiallyApply"

  it should "reduce arity 1 to 0" in {
    def render(s: String) = s == "Hello"

    val f = RenderableFunction(render _, "render")
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 0
  }

  it should "reduce arity 2 to 1" in {
    def render(s1: Scalar, s2: Scalar) = s1.render() + ":" + s2.render()

    val f = RenderableFunction(render _, "render")
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 1
  }

  it should "reduce arity 3 to 2" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render()

    val f = RenderableFunction(render _, "render")
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 2
  }

  it should "reduce arity 4 to 3" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar, s4: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render() + ":" + s4.render()

    val f = RenderableFunction(render _, "render")
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 3
  }

  behavior of "RenderableFunction.applyParameters"

  implicit def isConstant(s: Scalar): Boolean = s match {
    case StringScalar(_, _) => false
    case _ => true
  }

  val variables = Map("SLR.ACCOUNT" -> "x10177789", "k" -> "K", "l" -> "42", "?" -> "A")
  implicit val lookup: (String) => Option[String] = variables.get

  it should "reduce arity 1 to 0 (1)" in {
    def render(s: String) = s == "Hello"

    val f = RenderableFunction(render _, "render")
    val gy = f.applyParameters(List[Parameter[String]](Left("k")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
  }

  it should "reduce arity 1 to 0 (2)" in {
    def render(s: String) = s

    val f = RenderableFunction(render _, "render")
    val gy = f.applyParameters(List[Parameter[String]](Left("K")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("K")
  }

  it should "reduce arity 2 to 0 (a)" in {
    def render(s1: String, s2: String) = s"$s1:$s2"

    val f = RenderableFunction(render _, "render")
    val gy = f.applyParameters(List[Parameter[String]](Left("k"), Left("l")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("k:l")
  }

  it should "reduce arity 2 to 0 (b)" in {
    def render(s1: String, s2: String) = s"$s1:$s2"

    val f = RenderableFunction(render _, "render")
    val gy = f.applyParameters(List[Parameter[String]](Left("K"), Left("l")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("K:l")
  }

  it should "reduce arity 3 to 2" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render()

    val f = RenderableFunction(render _, "render")
    val gy = f.applyParameters(List[Parameter[String]](Left("k")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 2
  }

  it should "reduce arity 4 to 3" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar, s4: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render() + ":" + s4.render()

    val f = RenderableFunction(render _, "render")
    val gy = f.applyParameters(List[Parameter[String]](Left("k")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 3
  }

  it should "handle a function (1)" in {
    val map = Map("pi"->"3.1415927")
    val lookup: RenderableFunction[String] = RenderableFunction({ s: String => map(s)}, "lookup")

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render")
    val closure = Closure(lookup, Left("pi"))
    val gy = f.applyParameters(List[Parameter[String]](Right(closure)))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("3.1415927")
  }

  it should "handle a function (2)" in {
    val map = Map("pi"->"3.1415927")
    val lookup: RenderableFunction[String] = RenderableFunction({ s: String => map(s)}, "lookup")

    def show(s1: String) = s1.toDouble

    val f = RenderableFunction(show _, "render")
    val closure = Closure(lookup, Left("pi"))
    val gy = f.applyParameters(List[Parameter[String]](Right(closure)))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success(3.1415927)
  }

  it should "handle a function that throws an exception" in {
    val map = Map("PI"->"3.1415927")
    val lookup: RenderableFunction[String] = RenderableFunction({ s: String => map(s)}, "lookup")

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render")
    println(s"f: $f")
    val gy = f.applyParameters(List[Parameter[String]](Right(Closure(lookup, Left("pi")))))
    println(s"gy: $gy")
    gy should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  it should "deal with not" in {
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}

    val f = RenderableFunction(fNot _, "not")
    val gy = f.applyParameters(List[Parameter[Boolean]](Left(true)))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success(false)
  }

  it should "deal with NOT IN" in {
    val f_1: RenderableFunction[Boolean] = RenderableFunction({b: Boolean => println(s"call NOT $b"); !b}, "NOT")
    println(s"f_1: $f_1")
    f_1.arity shouldBe 1
    val g_2: RenderableFunction[Boolean] = RenderableFunction((p0: String, p1: List[String]) => {println(s"$p0 IN $p1"); p1 contains p0}, "IN")
    println(s"g_2: $g_2")
    g_2.arity shouldBe 2
    val g_2i = g_2.invert(2)
    val g__1y = g_2i.applyParameters(List[Parameter[Any]](Left(List("x", "y", "z"))))
    g__1y should matchPattern { case Success(_) => }
    val g__1 = g__1y.get
    println(s"g__1: $g__1")
    g__1.arity shouldBe 1
    val fg_2y = f_1.applyParameters(List[Parameter[Any]](Right(Closure(g__1, Left("x")))))
    fg_2y should matchPattern { case Success(_) => }
    val fg_2 = fg_2y.get
    println(s"fg_2: $fg_2")
    fg_2.callByName() shouldBe Success(false)
  }

  behavior of "RenderableFunction.untupled"

  it should "yield the same hashCode" in {
    def render(s: String) = s

    val rf1y = RenderableFunction(render _, "render").applyParameters(List(Left("k")))
    rf1y should matchPattern { case Success(_) => }
    val hash1 = rf1y.get.callByName().get.hashCode
    val rf2y = RenderableFunction(render _, "render").applyParameters(List(Left("k")))
    rf2y should matchPattern { case Success(_) => }
    val hash2 = rf2y.get.callByName().get.hashCode
    hash1 shouldEqual hash2
  }

  behavior of "FunctionString.invert"

  it should "yield unchanged FunctionString for n = 0 or 1" in {
    val fs2 = FunctionString("f", 2)
    fs2.invert(0) shouldBe fs2
    fs2.invert(1) shouldBe fs2
  }

  it should "yield switch first two args for n = 2" in {
    val fs2 = FunctionString("f", 2)
    fs2.invert(2).toString shouldBe "f(b?)(a?)"
    val fs3 = FunctionString("f", 3)
    fs3.invert(2).toString shouldBe "f(b?)(a?)(c?)"
    val fs4 = FunctionString("f", 4)
    fs4.invert(2).toString shouldBe "f(b?)(a?)(c?)(d?)"
  }

  it should "yield switch first three args for n = 3" in {
    val fs3 = FunctionString("f", 3)
    fs3.invert(3).toString shouldBe "f(c?)(b?)(a?)"
    val fs4 = FunctionString("f", 4)
    fs4.invert(3).toString shouldBe "f(c?)(b?)(a?)(d?)"
  }

  behavior of "RenderableFunction.invert"

  it should "handle the two-param version of render" in {
    val resultArity = 0
    def render(s1: String, s2: String) = s"$s1:$s2"

    val rf1 = RenderableFunction(render _, "render")
    val rf1i = rf1.invert(2)
    val rf2y = rf1i.applyParameters(List[Parameter[String]](Left("l"), Left("K")))
    rf2y should matchPattern { case Success(_) => }
    rf2y.get.arity shouldBe resultArity
    rf2y.get.callByName shouldBe Success("K:l")
  }

  behavior of "createVarArgsClosure"

  it should "handle varargs of cardinality 0" in {
    val c = Closure.createVarArgsClosure()
    c.arity shouldBe 0
    c() shouldBe Success(Seq())
  }

  it should "handle varargs of cardinality 1" in {
    val c = Closure.createVarArgsClosure("l")
    c.arity shouldBe 0
    c() shouldBe Success(Seq("l"))
  }

  it should "handle varargs of cardinality 2" in {
    val c = Closure.createVarArgsClosure("l", "K")
    c.arity shouldBe 0
    c() shouldBe Success(Seq("l", "K"))
  }

  behavior of "RenderableFunction.varargs"

  // NOTE that this is essentially a repeat of the tests immediately above.

  it should "handle varargs of cardinality 0" in {
    val rf1 = RenderableFunction.varargs(0)
    rf1.arity shouldBe 0
    println(rf1)
    val rf2y: Try[RenderableFunction[Seq[String]]] = rf1.applyParameters(List[Parameter[String]]())
    rf2y should matchPattern { case Success(_) => }
    println(rf2y.get)
    rf2y.get.callByName shouldBe Success(List())
  }

  it should "handle varargs of cardinality 1" in {
    val rf1 = RenderableFunction.varargs(1)
    rf1.arity shouldBe 1
    println(rf1)
    val rf2y: Try[RenderableFunction[Seq[String]]] = rf1.applyParameters(List[Parameter[String]](Left("l")))
    rf2y should matchPattern { case Success(_) => }
    println(rf2y.get)
    rf2y.get.callByName shouldBe Success(List("l"))
  }

  it should "handle varargs of cardinality 2" in {
    val rf1 = RenderableFunction.varargs(2)
    rf1.arity shouldBe 2
    println(rf1)
    val rf2y: Try[RenderableFunction[Seq[String]]] = rf1.applyParameters(List[Parameter[String]](Left("l"), Left("K")))
    rf2y should matchPattern { case Success(_) => }
    println(rf2y.get)
    rf2y.get.callByName shouldBe Success(List("l", "K"))
  }

}
