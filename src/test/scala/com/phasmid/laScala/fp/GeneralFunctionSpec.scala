package com.phasmid.laScala.fp

import com.phasmid.laScala.values.{BooleanScalar, QuotedStringScalar, Scalar, StringScalar}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scala.util._

/**
  * Created by scalaprof on 10/19/16.
  */
class GeneralFunctionSpec extends FlatSpec with Matchers {

  val s2s: Scalar => Option[String] = { s => Option(s.render()) }
  val s2b: Scalar => Option[Boolean] = { s => s.asBoolean }

  behavior of "FunctionString"
  it should "add n parameters" in {
    FunctionString("f",0).toString shouldBe "f"
    FunctionString("f",1).toString shouldBe "f(a?)"
    FunctionString("f",2).toString shouldBe "f(a?)(b?)"
    FunctionString("f",26).toString shouldBe "f(a?)(b?)(c?)(d?)(e?)(f?)(g?)(h?)(i?)(j?)(k?)(l?)(m?)(n?)(o?)(p?)(q?)(r?)(s?)(t?)(u?)(v?)(w?)(x?)(y?)(z?)"
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
  it should "partiallyApply correctly with index 0" in {
    FunctionString("f", 1).partiallyApply("x").toString shouldBe """f("x")"""
    FunctionString("f",2).partiallyApply("x").toString shouldBe """f("x")(b?)"""
    an [GeneralFunctionException] should be thrownBy FunctionString("f",0).partiallyApply("x")
  }
  it should "partiallyApply correctly with index 1" in {
    FunctionString("f",2).partiallyApply("x",1).toString shouldBe """f(a?)("x")"""
    an [GeneralFunctionException] should be thrownBy FunctionString("f",0).partiallyApply("x",1)
    an [GeneralFunctionException] should be thrownBy FunctionString("f",1).partiallyApply("x",1)
  }

  behavior of "GeneralFunction.apply"
  it should "yield Hello with arity 1" in {
    def render(s: String) = s == "Hello"

    val f = GeneralFunction.asTupledFunctionType(render _)
    val fc = GeneralFunction(List("p1").size, f, "render")
    val r = fc.apply(Tuple1("Hello"))
    r should matchPattern { case true => }
  }
  it should "yield Hello with arity 2" in {
    def render(s1: Scalar, s2: Scalar) = s1.render()

    val f = GeneralFunction.asTupledFunctionType(render _)
    val fc = GeneralFunction(List("p1", "p2").size, f, "render")
    val r = fc.apply(Tuple2(Scalar("Hello"), Scalar(1)))
    r should matchPattern { case "Hello" => }
  }
  it should "yield Hello with arity 3" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render()

    val f = GeneralFunction.asTupledFunctionType(render _)
    val fc = GeneralFunction(List("p1", "p2", "p3").size, f, "render")
    val r = fc.apply(Tuple3(Scalar("Hello"), Scalar(1), Scalar(true)))
    r should matchPattern { case "Hello" => }
  }
  behavior of "GeneralFunction.partiallyApply"
  it should "reduce arity 1 to 0" in {
    val resultArity = 0
    def render(s: String) = s == "Hello"

    val fc1 = GeneralFunction(List("p1").size, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2: GeneralFunction[Boolean] = fc1.partiallyApply(Some("Hello"))
    fc2.arity shouldBe resultArity
  }
  it should "reduce arity 2 to 1" in {
    def render(s1: Scalar, s2: Scalar) = s1.render()

    val fc1 = GeneralFunction(List("p1", "p2").size, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2: GeneralFunction[String] = fc1.partiallyApply(Some("Hello"))
    fc2.arity shouldBe 1
  }
  it should "reduce arity 3 to 2" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render()

    val fc1 = GeneralFunction(List("p1", "p2", "p3").size, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2: GeneralFunction[String] = fc1.partiallyApply(Some("Hello"))
    fc2.arity shouldBe 2
  }
  it should "reduce arity 4 to 3" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar, s4: Scalar) = s1.render()

    val fc1 = GeneralFunction(List("p1", "p2", "p3", "p4").size, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2: GeneralFunction[String] = fc1.partiallyApply(Some("Hello"))
    fc2.arity shouldBe 3
  }

  behavior of "GeneralFunction.applyParameters"

  implicit def isConstant(s: Scalar): Boolean = s match {
    case StringScalar(_, _) => false
    case _ => true
  }

  val variables = Map("SLR.ACCOUNT" -> "x10177789", "k" -> "K", "l" -> "42", "?" -> "A")
  implicit val lookup: (String) => Option[String] = variables.get

  it should "reduce arity 1 to 0 (1)" in {
    val resultArity = 0
    def render(s: String) = s == "Hello"

    val fc1 = GeneralFunction(1, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2y = fc1.applyParameters(List[Parameter[String]](Left("k")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
  }
  it should "reduce arity 1 to 0 (2)" in {
    val resultArity = 0
    def render(s: String) = s

    val fc1 = GeneralFunction(1, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2y = fc1.applyParameters(List[Parameter[String]](Left("K")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
    fc2y.get.untupled() shouldBe "K"
  }
  it should "reduce arity 2 to 0 (a)" in {
    val resultArity = 0
    def render(s1: String, s2: String) = s"$s1:$s2"

    val fc1 = GeneralFunction(2, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2y = fc1.applyParameters(List[Parameter[String]](Left("k"), Left("l")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
    fc2y.get.untupled() shouldBe "k:l"
  }
  it should "reduce arity 2 to 0 (b)" in {
    val resultArity = 0
    def render(s1: String, s2: String) = s"$s1:$s2"

    val fc1 = GeneralFunction(2, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2y = fc1.applyParameters(List[Parameter[String]](Left("K"), Left("l")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
    fc2y.get.untupled() shouldBe "K:l"
  }
  it should "reduce arity 3 to 2" in {
    def render(s1: Scalar, s2: Scalar, s3: Scalar) = s1.render()
    val resultArity = 2
    val fc1 = GeneralFunction(3, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2y = fc1.applyParameters(List[Parameter[String]](Left("k")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
  }
  it should "reduce arity 4 to 3" in {
    val resultArity = 3
    def render(s1: Scalar, s2: Scalar, s3: Scalar, s4: Scalar) = s1.render()

    val fc1 = GeneralFunction(4, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc2y = fc1.applyParameters(List[Parameter[String]](Left("k")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
  }

  it should "deal with not" in {
    val resultArity = 0
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}
    val paramsNot = List("boolean")
    val arity = paramsNot.size
    val fc1 = GeneralFunction(arity, GeneralFunction.asTupledFunctionType(fNot _), "not")
    val fc2y = fc1.applyParameters(List[Parameter[Boolean]](Left(true)))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
    fc2y.get.untupled() shouldBe false
  }

  it should "deal with not and varargs" in {
    val resultArity = 0
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}
    val paramsNot = List("boolean")
    val arityNot = paramsNot.size
    val fc1 = GeneralFunction(arityNot, GeneralFunction.asTupledFunctionType(fNot _), "not")
    def fIn(p0: String, p1: List[String]): Boolean = p1.contains(p0)
    val paramsIn = List("x", "list")
    val convertersIn = Seq(s2s, s2s)
    val arityIn = paramsIn.size
    // TODO resurrect this part of the test
//    val fc1 = GeneralFunction(arityIn, GeneralFunction.asTupledFunctionType(fIn _), "in")
//    val fc2y = fc1.applyParameters(List[Parameter](Left("TRUE")), resultArity)
//    println(fc2y)
//    fc2y should matchPattern { case Success(_) => }
//    fc2y.get.arity shouldBe resultArity
//    fc2y.get.untupled() shouldBe false
  }

  behavior of "GeneralFunction.untupled"

  it should "yield the same hashCode" in {
    def render(s: String) = s

    val fc1y = GeneralFunction(1, GeneralFunction.asTupledFunctionType(render _), "render").applyParameters(List(Left("k")))
    fc1y should matchPattern { case Success(_) => }
    val hash1 = fc1y.get.untupled().hashCode
    val fc2y = GeneralFunction(1, GeneralFunction.asTupledFunctionType(render _), "render").applyParameters(List(Left("k")))
    fc2y should matchPattern { case Success(_) => }
    val hash2 = fc2y.get.untupled().hashCode
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

  behavior of "GeneralFunction.invert"
  it should "handle the two-param version of render" in {
    val resultArity = 0
    def render(s1: String, s2: String) = s"$s1:$s2"
    val fc1 = GeneralFunction(2, GeneralFunction.asTupledFunctionType(render _), "render")
    val fc1i = fc1.invert(2)
    val fc2y = fc1i.applyParameters(List[Parameter[String]](Left("l"), Left("K")))
    fc2y should matchPattern { case Success(_) => }
    fc2y.get.arity shouldBe resultArity
    fc2y.get.untupled() shouldBe "K:l"
  }

}
