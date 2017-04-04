/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.fp.RenderableFunction.getPartialApplicationFunction
import com.phasmid.laScala.values.{BooleanScalar, Scalar, StringScalar, Tuple0}
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Left, _}

/**
  * Created by scalaprof on 10/19/16.
  */
class RenderableFunctionSpec extends FlatSpec with Matchers with PrivateMethodTester {

  behavior of "RenderableFunction"

  it should "yield true with arity 0" in {
    def render() = true

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(0))
    val r = f(Tuple0)
    r should matchPattern { case Success(true) => }
  }

  it should "yield Hello with arity 1" in {
    def render(s: String) = s == "Hello"

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val r = f(Tuple1("Hello"))
    r should matchPattern { case Success(true) => }
  }

  it should "yield Hello with arity 2" in {
    def render(s1: Scalar, s2: Scalar) = s1.render() + ":" + s2.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(2))
    val r = f(Tuple2(Scalar("Hello"), Scalar(1)))
    r should matchPattern { case Success("Hello:1") => }
  }

  it should "yield Hello with arity 3" in {
    // XXX Not sure why we have to set this here, but we do.
    BooleanScalar.setDefaultFormat("%b")

    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(3))
    val r = f(Tuple3(Scalar("Hello"), Scalar(1), Scalar(true)))
    val hello1true = "Hello:1:true"
    r should matchPattern { case Success(`hello1true`) => }
  }

  it should "handle function returning a Try" in {
    val map = Map("x" -> "X")

    def lookup(s: String): Try[String] = FP.optionToTry(map.get(s))

    // TODO this should use a call-by-name parameter
    val f = RenderableFunction(lookup _, "lookup", RenderableFunction.callByName(1))
    val r = f(Tuple1("x"))
    r should matchPattern { case Success("X") => }
  }

  it should "apply a 1-arity function" in {
    val name = "isHello"
    val arity = 1
    val f = RenderableFunction({ s: String => s == "Hello" }, name, RenderableFunction.callByValue(1))
    f.w shouldBe FunctionString(name, arity)
    f.arity shouldBe arity
    f.func(Tuple1("Hello")) shouldBe true
    an[ClassCastException] should be thrownBy f.func("Hello")
  }

  it should "apply a 2-arity function" in {
    val name = "render"
    val arity = 2

    def render(s1: String, s2: String) = s1 + ":" + s2

    val f = RenderableFunction(render _, name, RenderableFunction.callByValue(2))
    f.w shouldBe FunctionString(name, arity)
    f.arity shouldBe arity
    f.func(("Hello", "Goodbye")) shouldBe "Hello:Goodbye"
    an[ClassCastException] should be thrownBy f.func("Hello")
    an[ClassCastException] should be thrownBy f.func(Tuple1("Hello"))
  }

  it should "work with partiallyApply (1)" in {
    val name = "render"

    def render(s1: String, s2: String) = s1 + ":" + s2

    val f = RenderableFunction(render _, name, RenderableFunction.callByValue(2))
    val g1y: Try[RenderableFunction[String]] = f.partiallyApply("Hello")
    for (g1 <- g1y) yield g1.arity should matchPattern { case Success(1) => }
    val g2y: Try[RenderableFunction[String]] = for (g <- g1y; h <- g.partiallyApply("Goodbye")) yield h
    (for (g2 <- g2y) yield g2.arity) should matchPattern { case Success(0) => }
    (for (g2 <- g2y; r <- g2.callByName()) yield r) should matchPattern { case Success("Hello:Goodbye") => }
  }

  it should "work with partiallyApply (2)" in {
    val name = "and"
    def and(x: Boolean, y: => Boolean) = x && y

    val f = RenderableFunction(and _, name, Seq(false, true))
    val bt: Boolean = true
    val bf: Boolean = false
    val g1y: Try[RenderableFunction[Boolean]] = f.partiallyApply(bt)
    val g2y: Try[RenderableFunction[Boolean]] = for (g <- g1y; h <- g.partiallyApply(bf)) yield h
    val by: Try[Boolean] = for (g2 <- g2y; b <- g2.callByName()) yield b
    by should matchPattern { case Success(false) => }
  }

  it should "work with partiallyApply (3)" in {
    val name = "and"

    def and(x: String, y: => String) = x.toBoolean && y.toBoolean

    val f = RenderableFunction(and _, name, Seq(false, true))
    val g1y: Try[RenderableFunction[Boolean]] = f.partiallyApply("true")
    val g2y: Try[RenderableFunction[Boolean]] = for (g <- g1y; h <- g.partiallyApply("false")) yield h
    val by: Try[Boolean] = for (g2 <- g2y; b <- g2.callByName()) yield b
    by should matchPattern { case Success(false) => }
  }

  it should "work with partiallyApply (4)" in {
    val name = "and"

    def and(x: String, y: => String) = (x.toBoolean && y.toBoolean).toString

    val f = RenderableFunction(and _, name, Seq(false, true))
    val g1y: Try[RenderableFunction[String]] = f.partiallyApply("true")
    val g2y: Try[RenderableFunction[String]] = for (g <- g1y; h <- g.partiallyApply("false")) yield h
    val by: Try[String] = for (g2 <- g2y; b <- g2.callByName()) yield b
    by should matchPattern { case Success("false") => }
  }

  behavior of "render"

  it should "give function string with tabs" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name, RenderableFunction.callByValue(1))
    f.render(1) shouldBe s"$name(a?)"
  }

  behavior of "Param"
  it should "bind correctly" in {
    val f = FunctionString("f", 1)
    val p = f.ps.head
    p.bind("x") shouldBe Param("x")
    val g = FunctionString("g", 2)
    val p1 = g.ps.head
    val p2 = g.ps.tail.head
    p1.bind("x") shouldBe Param("x")
    p2.bind("y") shouldBe Param("y")
  }

  behavior of "FunctionString"

  it should "add n parameters" in {
    FunctionString("f", 0).toString shouldBe "f"
    FunctionString("f", 1).toString shouldBe "f(a?)"
    FunctionString("f", 2).toString shouldBe "f(a?)(b?)"
    FunctionString("f", 2, Seq(false,true)).toString shouldBe "f(a?)(=>b?)"
    FunctionString("f", 26).toString shouldBe "f(a?)(b?)(c?)(d?)(e?)(f?)(g?)(h?)(i?)(j?)(k?)(l?)(m?)(n?)(o?)(p?)(q?)(r?)(s?)(t?)(u?)(v?)(w?)(x?)(y?)(z?)"
  }

  it should "use custom parameter names" in {
    FunctionString.custom("f", List("x")).toString shouldBe "f(x?)"
    FunctionString.custom("f", List("x", "mean")).toString shouldBe "f(x?)(mean?)"
  }

  it should "use custom parameter names with call-by-name" in {
    FunctionString.custom("f", List("x"), Seq(true)).toString shouldBe "f(=>x?)"
    FunctionString.custom("f", List("x", "mean"), Seq(false,true)).toString shouldBe "f(x?)(=>mean?)"
  }

  it should "calculate arity correctly" in {
    FunctionString("f", 0).arity shouldBe 0
    FunctionString("f", 1).arity shouldBe 1
    FunctionString("f", 2).arity shouldBe 2
    FunctionString("f(1)", 0).arity shouldBe 0
    FunctionString("f(true)", 1).arity shouldBe 1
    FunctionString("f(3.1415927)", 2).arity shouldBe 2
    FunctionString("""f("x")""", 0).arity shouldBe 0
    FunctionString("""f("y")""", 1).arity shouldBe 1
    FunctionString("""f("z")""", 2).arity shouldBe 2
  }

  it should "calculate arity correctly for function" in {
    val f = FunctionString("f", 1)
    f.arity shouldBe 1
    val g = FunctionString("g", 1)
    g.arity shouldBe 1
    val h = g.partiallyApplyFunction(f)
    h.arity shouldBe 1
  }

  it should "bind correctly" in {
    val f = FunctionString("f", 1)
    f.bind(0, "x") shouldBe Seq(Param("x"))
    val g = FunctionString("g", 2)
    g.bind(0, "x") shouldBe Seq(Param("x"), Param(FreeParam('b')))
    g.bind(1, "y") shouldBe Seq(Param(FreeParam('a')), Param("y"))
  }

  it should "partiallyApply correctly a two-param function" in {
    val g = FunctionString("f", 2).partiallyApply("x").partiallyApply("y")
    g.arity shouldBe 0
    g.toString shouldBe """f("x")("y")"""
  }


  it should "partiallyApply correctly with index 0" in {
    FunctionString("f", 1).partiallyApply("x").toString shouldBe """f("x")"""
    FunctionString("f", 2).partiallyApply("x").toString shouldBe """f("x")(b?)"""
    an[RenderableFunctionException] should be thrownBy FunctionString("f", 0).partiallyApply("x")
  }

  it should "partiallyApply correctly with index 1" in {
    FunctionString("f", 2).partiallyApply("x", 1).toString shouldBe """f(a?)("x")"""
    an[RenderableFunctionException] should be thrownBy FunctionString("f", 0).partiallyApply("x", 1)
    an[RenderableFunctionException] should be thrownBy FunctionString("f", 1).partiallyApply("x", 2)
  }

  it should "partiallyApply a function" in {
    val w1 = FunctionString("f", 1)
    val w2 = FunctionString("concat", 1, Seq(false))
    val w3 = w1.partiallyApplyFunction(w2)
    val w4 = w3.partiallyApply("x")
    w4.toString shouldBe "f(concat(\"x\"))"
    w1.arity shouldBe 1
    w2.arity shouldBe 1
    w3.arity shouldBe 1
    w4.arity shouldBe 0
  }

  it should "partiallyApply a function (2)" in {
    val w1 = FunctionString("f", 1)
    val w2 = FunctionString("concat", 1, Seq(false))
    val w3 = w1.partiallyApplyFunction(w2)
    val w4 = w3.partiallyApply("x")
    val w2a = w2.partiallyApply("x")
    val w3a = w1.partiallyApplyFunction(w2a)
    w2.toString shouldBe "concat(a?)"
    w3.toString shouldBe "f(concat(a?))"
    w4.toString shouldBe "f(concat(\"x\"))"
    w2a.toString shouldBe "concat(\"x\")"
    w3a.toString shouldBe "f(concat(\"x\"))"
    w1.arity shouldBe 1
    w2.arity shouldBe 1
    w3.arity shouldBe 1
    w4.arity shouldBe 0
    w2a.arity shouldBe 0
    w3a.arity shouldBe 0
  }

  it should "partiallyApplyFunction correctly" in {
    // TODO understand why this is a bit flaky
    val f = FunctionString("f", 1)
    val g = FunctionString("g", 1)
    val h = g.partiallyApplyFunction(f)
    h.arity shouldBe 1
    h.toString shouldBe """g(f(a?))"""
  }

  behavior of "partiallyApply"

  it should "reduce arity 1 to 0" in {
    def render(s: String) = s == "Hello"

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 0
  }

  it should "reduce arity 2 to 1" in {
    def render(s1: Scalar, s2: Scalar) = s1.render() + ":" + s2.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(2))
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 1
  }

  it should "reduce arity 3 to 2" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(3))
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 2
  }

  it should "reduce arity 4 to 3" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar, s4: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render() + ":" + s4.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(4))
    val g = f.partiallyApply(Some("Hello"))
    g should matchPattern { case Success(_) => }
    g.get.arity shouldBe 3
  }

  behavior of "applyAllParameters"

  implicit def isConstant(s: Scalar): Boolean = s match {
    case StringScalar(_, _) => false
    case _ => true
  }

  val variables = Map("SLR.ACCOUNT" -> "x10177789", "k" -> "K", "l" -> "42", "?" -> "A")
  implicit val lookup: (String) => Option[String] = variables.get

  it should "reduce arity 1 to 0 (1)" in {
    def render(s: String) = s == "Hello"

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val gy = f.partiallyApplyParameters(List[Parameter[_]](Left("k")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
  }

  it should "reduce arity 1 to 0 (2)" in {
    def render(s: String) = s

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val gy = f.partiallyApplyParameters(List[Parameter[_]](Left("K")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("K")
  }

  it should "reduce arity 2 to 0 (a)" in {
    def render(s1: String, s2: String) = s"$s1:$s2"

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(2))
    val gy = f.partiallyApplyParameters(List[Parameter[_]](Left("k"), Left("l")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("k:l")
  }

  it should "reduce arity 2 to 0 (b)" in {
    def render(s1: String, s2: String) = s"$s1:$s2"

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(2))
    val gy = f.partiallyApplyParameters(List[Parameter[_]](Left("K"), Left("l")))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success("K:l")
  }

  it should "reduce arity 3 to 2" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(3))
    val gy = f.partiallyApplyParameters(List[Parameter[_]](Left(Scalar("k"))))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 2
  }

  it should "reduce arity 4 to 3" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar, s4: Scalar) = s1.render() + ":" + s2.render() + ":" + s3.render() + ":" + s4.render()

    val f = RenderableFunction(render _, "render", RenderableFunction.callByValue(4))
    val gy = f.partiallyApplyParameters(List[Parameter[_]](Left(Scalar("k"))))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 3
  }

  it should "handle a function (1)" in {
    // First we give a bad value of pi
    val map = mutable.HashMap("pi" -> "22/7")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val c1 = Closure(lookup, Left("pi"))
    val c2 = Closure(f, Right(c1))
    val c3y = c2.partiallyApply
    c3y should matchPattern { case Success(_) => }
    c3y.get.arity shouldBe 0

    // At this point, c4y does not "close" over the value of pi, it is still to be evaluated which means that we can set the new, proper value, before we apply c4y.get
    map.put("pi", "3.1415927")
    val wy = for (c3 <- c3y; c4 <- c3.callByName()) yield c4
    wy shouldBe Success("3.1415927")
  }

  it should "handle a function (2)" in {
    val map = Map("pi" -> "3.1415927")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1.toDouble

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val c1 = Closure(lookup, Left("pi"))
    val c2 = Closure(f, Right(c1))
    val xfy: Try[RenderableFunction[Double]] = c2.partiallyApply
    xfy should matchPattern { case Success(_) => }
    xfy.get.arity shouldBe 0
    xfy.get.callByName() shouldBe Success(3.1415927)
  }

  it should "handle a function that throws an exception (1)" in {
    val map = Map("PI" -> "3.1415927")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val fy: Try[RenderableFunction[String]] = f.partiallyApplyParameters(Seq(Right(Closure(lookup, Left("pi")))))
    val ry = for (g: RenderableFunction[String] <- fy; r <- g.callByName()) yield r
    ry should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  ignore should "handle a function that throws an exception 2)" in {
    val map = Map("PI" -> "3.1415927")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val cy = f.partiallyApplyParameters(List[Parameter[String]](Right(Closure(lookup, Left("pi")))))
    cy should matchPattern { case Failure(_: RenderableFunctionException) => }
  }

  it should "deal with not" in {
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}

    val f = RenderableFunction(fNot _, "not", RenderableFunction.callByValue(1))
    val gy = f.partiallyApplyParameters(List[Parameter[Boolean]](Left(true.booleanValue)))
    gy should matchPattern { case Success(_) => }
    gy.get.arity shouldBe 0
    gy.get.callByName shouldBe Success(false)
  }

  it should "deal with NOT IN" in {
    val f_1: RenderableFunction[Boolean] = RenderableFunction({ b: Boolean => println(s"call NOT $b"); !b }, "NOT", RenderableFunction.callByValue(1))
    f_1.arity shouldBe 1
    val g_2: RenderableFunction[Boolean] = RenderableFunction((p0: String, p1: List[String]) => {
      println(s"$p0 IN $p1")
      p1 contains p0
    }, "IN", RenderableFunction.callByValue(2))
    g_2.arity shouldBe 2
    val g_2i = g_2.invert(2)
    val g__1y = g_2i.partiallyApplyParameters(List[Parameter[List[String]]](Left(List("x", "y", "z"))))
    g__1y should matchPattern { case Success(_) => }
    val g__1 = g__1y.get
    g__1.arity shouldBe 1
    val fg_2y = f_1.partiallyApplyParameters(List[Parameter[Boolean]](Right(Closure(g__1, Left("x")))))
    fg_2y should matchPattern { case Success(_) => }
    val fg_2 = fg_2y.get
    fg_2.callByName() shouldBe Success(false)
  }

  behavior of "hashCode"

  it should "yield the same value for two simple functions" in {
    def render(s: String) = s

    val f1 = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val f2 = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    f1.hashCode shouldEqual f2.hashCode
    f1 shouldEqual f2
  }
  it should "yield the same value when partially applied" in {
    def render(s: String) = s

    val f1 = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val f2 = RenderableFunction(render _, "render", RenderableFunction.callByValue(1))
    val rf1y: Try[RenderableFunction[String]] = f1.partiallyApply("k")
    rf1y should matchPattern { case Success(_) => }
    val rf2y: Try[RenderableFunction[String]] = f2.partiallyApply("k")
    rf2y should matchPattern { case Success(_) => }
    rf1y.get.hashCode() shouldEqual rf2y.get.hashCode()
    (rf1y.get == rf2y.get) shouldBe true
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

  behavior of "invert"

  //  it should "perform the invert properly" in {
  //    val resultArity = 0
  //
  //    def render(s1: String, s2: => String) = s"$s1:$s2"
  //
  //    val rf1 = RenderableFunction(render _, "render", Seq(false, true))
  //    val rf1i = rf1.invert(2)
  //    val rf2y = rf1i.applyAllParameters(List[Parameter[String]](Left("l"), Left("K")))
  //    rf2y should matchPattern { case Success(_) => }
  //    rf2y.get.arity shouldBe resultArity
  //  }

  //  it should "handle the two-param version of render" in {
  //    val resultArity = 0
  //
  //    def render(s1: String, s2: String) = s"$s1:$s2"
  //
  //    val rf1 = RenderableFunction(render _, "render", Seq(false, false))
  //    val rf1i = rf1.invert(2)
  //    val rf2y = rf1i.applyAllParameters(List[Parameter[String]](Left("l"), Left("K")))
  //    rf2y should matchPattern { case Success(_) => }
  //    rf2y.get.arity shouldBe resultArity
  //    rf2y.get.callByName shouldBe Success("K:l")
  //  }

  behavior of "varargs"

  // NOTE that this is essentially a repeat of the tests immediately above.

  it should "handle varargs of cardinality 0" in {
    val rf1: RenderableFunction[Seq[String]] = RenderableFunction.varargs(0)
    rf1.arity shouldBe 0
    val rf2y = rf1.partiallyApplyParameters(List[Parameter[String]]())
    rf2y should matchPattern { case Success(_) => }
    rf2y.get.callByName shouldBe Success(List())
  }

  it should "handle varargs of cardinality 1" in {
    val rf1: RenderableFunction[Seq[Seq[String]]] = RenderableFunction.varargs(1)
    rf1.arity shouldBe 1
    val rf2y = rf1.partiallyApplyParameters(List[Parameter[String]](Left("l")))
    rf2y should matchPattern { case Success(_) => }
    rf2y.get.callByName shouldBe Success(List("l"))
  }

  it should "handle varargs of cardinality 2" in {
    val rf1 = RenderableFunction.varargs(2)
    rf1.arity shouldBe 2
    val rf2y = rf1.partiallyApplyParameters(List[Parameter[String]](Left("l"), Left("K")))
    rf2y should matchPattern { case Success(_) => }
    rf2y.get.callByName shouldBe Success(List("l", "K"))
  }

  behavior of "boolean expression"
  it should """evaluate to true with true & true""" in {
    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", Seq(false, true))
    val cAnd = Closure(ifAnd, Left(true.booleanValue), Left(true.booleanValue))
    cAnd() match {
      case Success(b) => println(s"result: $b"); succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }
  it should """evaluate to true with true & (java) true""" in {
    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", Seq(false, true))
    val cAnd = Closure(ifAnd, Left(true.booleanValue), Left(true))
    cAnd() match {
      case Success(b) => println(s"result: $b"); succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }
  it should """evaluate (using java Boolean parameter) to true with true & (java) true""" in {
    def fAnd(p1: Boolean, p2: => java.lang.Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", Seq(false, true))
    val cAnd = Closure(ifAnd, Left(true), Left(true))
    cAnd() match {
      case Success(b) => println(s"result: $b"); succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }
  it should """evaluate to java.lang.Boolean(true) with true & (java) true""" in {
    def fAnd(p1: java.lang.Boolean, p2: => java.lang.Boolean): java.lang.Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", Seq(false, true))
    val cAnd = Closure(ifAnd, Left(true), Left(true))
    cAnd() match {
      case Success(b) => println(s"result: $b"); succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  // TODO investigate why this doesn't work
  ignore should """evaluate to java.lang.Boolean(true) with true & not (java) true""" in {
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}

    val ifNot = RenderableFunction(fNot _, "not", RenderableFunction.callByValue(1))

    def fAnd(p1: java.lang.Boolean, p2: => java.lang.Boolean): java.lang.Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", Seq(false, true))
    val cAnd = Closure(ifAnd, Left(true), Right(Closure(ifNot, Left(false))))
    println(cAnd)
    val g = cAnd.partiallyApply
    println(s"g=$g")
    cAnd() match {
      case Success(b) => println(s"result: $b"); succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  // TODO investigate why this doesn't work
  ignore should """evaluate to true with true & not true""" in {
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}

    val ifNot = RenderableFunction(fNot _, "not", RenderableFunction.callByValue(1))

    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", Seq(false, true))
    val cAnd = Closure(ifAnd, Left(true), Right(Closure(ifNot, Left(false))))
    println(cAnd)
    val g = cAnd.partiallyApply
    println(s"g=$g")
    cAnd() match {
      case Success(b) => println(s"result: $b"); succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }


  behavior of "partiallyApply"
  def testPartiallyApply1[T1: ClassTag, R: ClassTag](expected: R, t1: T1, f: Product=>R, w: String, cbn: Boolean*) = {
    val arity = 1
    require(cbn.length==arity)
    val functionString = FunctionString(w, arity, cbn)
    val classTags = Seq(implicitly[ClassTag[T1]])
    assert(classTags.length==arity)
//    val privateMethod = PrivateMethod[Try[RenderableFunction[R]]]('partiallyApply)
//    val h = RenderableFunction invokePrivate privateMethod(arity, f, t1, functionString, cbn, classTags)
//    println(h)
    val g2y = RenderableFunction.partiallyApply[T1,R](arity, f, t1, functionString, cbn, classTags)
    g2y should matchPattern { case Success(_) => }
    g2y.get.arity shouldBe arity-1
    println(g2y.get.callByName())
    if (arity==1) g2y.get.callByName() should matchPattern { case Success(`expected`) => }
  }
  it should "work for partiallyApply1" in {
    testPartiallyApply1[Int,String]("(42)", 42, p => p.toString, "toString", false)
  }

  behavior of "getPartialApplicationFunction"
  def testGetPartialApplicationFunction1[T1: ClassTag](expected: String, t1: T1, f: Product=>String, cbn: Boolean) = {
    val arity = 1
//    val _g = PrivateMethod[Product=>String]('getPartialApplicationFunction)
//    val h = RenderableFunction invokePrivate _g(arity, f, t1, cbn)
//    h(Tuple0) shouldBe expected
    val g = getPartialApplicationFunction(arity, f, t1, cbn)
    g(Tuple0) shouldBe expected
  }
  def testGetPartialApplicationFunction2[T1: ClassTag](t1: T1, f: Product=>String, cbn: Boolean): Product=>String = {
    val arity = 2
//    val _g = PrivateMethod[Product=>String]('getPartialApplicationFunction)
//    RenderableFunction invokePrivate _g(arity, f, t1, cbn)
//    _g(arity, f, t1, cbn)
    val g = getPartialApplicationFunction(arity, f, t1, cbn)
    g
  }
  it should "work for partiallyApply1" in {
    testGetPartialApplicationFunction1[Int]("(42)", 42, p => p.toString, false)
  }
  it should "work for partiallyApply2a" in {
    val f = testGetPartialApplicationFunction2[Int](6, {case (x: Int, y: Int) => (x*y).toString}, false)
    testGetPartialApplicationFunction1[Int]("42", 7, f, false)
  }
  it should "work for partiallyApply2b" in {
    val f = testGetPartialApplicationFunction2[Int](7, {case (x: Int, y: Int) => (x-y).toString}, false)
    testGetPartialApplicationFunction1[Int]("1", 6, f, false)
  }
}
