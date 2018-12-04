/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.Scalar
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.Logger

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Left, _}

/**
  * Created by scalaprof on 10/19/16.
  */
class ClosureSpec extends FlatSpec with Matchers {

  behavior of "Closure"
  it should "apply for a simple closure with no params" in {
    val name = "Hello"
    val f1: () => String = { () => "Hello" }
    val f = RenderableFunction(f1, name, RenderableFunction.callByValue(0))
    val c = Closure(f)
    c.arity shouldBe 0
    c() shouldBe Success("Hello")
  }

  it should "apply for a simple closure" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name, RenderableFunction.callByValue(1))
    val c = Closure(f, Left("Hello"))
    c.arity shouldBe 0
    c() shouldBe Success(true)
  }

  ignore should "apply for a simple call-by-name closure" in {
    val name = "isHello"

    def isHello(s: => String): Boolean = s == "Hello"

    val f = RenderableFunction(isHello _, name, RenderableFunction.callByName(1))
    val c = Closure(f, Left("Hello"))
    c.arity shouldBe 0
    c() shouldBe Success(true)
  }

  // CONSIDER investigate why this doesn't work
  ignore should "apply for a closure where the parameter is itself a Closure" in {
    val getPi: Product => String = { _ => math.Pi.toString }
    val f1 = RenderableFunction({ s: String => s == "Hello" }, "isHello", RenderableFunction.callByValue(1))
    val f2: RenderableFunction[String] = RenderableFunction(0, FunctionString("pi", Nil), Nil, Nil)(getPi)
    val c1 = Closure(f2)
    val c2 = Closure(f1, Right(c1))
    c2.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),Boolean](1,  isHello(a?)) \n  Right(Closure(RenderableFunction[List(),java.lang.String](0,  pi) \n      \n    ))\n)"
    c2.arity shouldBe 0
    c2() shouldBe Success(false)
  }

  ignore should "apply for a closure where the call-by-name parameter is itself a Closure" in {
    //    val getPi: Product => String = { _: () => math.Pi.toString }
    def getPi: String = math.Pi.toString

    def isHello(s: => String): Boolean = s == "Hello"

    val f1 = RenderableFunction(isHello _, "isHello", RenderableFunction.callByName(1))
    val f2 = RenderableFunction(getPi _, "pi", Seq[Boolean]())
    val c1 = Closure(f2)
    val c2 = Closure(f1, Right(c1))
    c2.arity shouldBe 0
    c2() shouldBe Success(false)
  }

  it should "implement bind" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name, RenderableFunction.callByValue(1))
    val c1 = Closure[String, Boolean](f)
    c1.arity shouldBe 1
    val c2 = c1.bind(Left("Hello"))
    c2.arity shouldBe 0
    c2() shouldBe Success(true)
  }

  it should "throw exception where there is no parameter" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name, RenderableFunction.callByValue(1))
    val c = Closure(f)
    c.arity shouldBe 1
    c() should matchPattern { case Failure(_) => }
    an[RenderableFunctionException] should be thrownBy c().get
  }

  behavior of "createVarArgsClosure"

  it should "handle varargs of 0 elements" in {
    val c = Closure.createVarArgsClosure()
    c.render() shouldBe "Closure(RenderableFunction[List(),scala.collection.Seq](0,  mkList), )"
    c.arity shouldBe 0
    c() shouldBe Success(Seq())
  }

  it should "handle varargs of 1 constant elements" in {
    val c = Closure.createVarArgsClosure(Left("l"))
    c.render() shouldBe "Closure(RenderableFunction[List(),scala.collection.Seq](0,  mkList), )"
    c.arity shouldBe 0
    c() shouldBe Success(Seq("l"))
  }

  it should "handle varargs of 2 constant elements" in {
    val c = Closure.createVarArgsClosure(Left("l"), Left("K"))
    c.render() shouldBe "Closure(RenderableFunction[List(),scala.collection.Seq](0,  mkList), )"
    c.arity shouldBe 0
    c() shouldBe Success(Seq("l", "K"))
  }

  it should "handle varargs of 1 variable elements sandwiched in 2 constant elements" in {
    val f = RenderableFunction({ s: String => s.toUpperCase }, "upper", RenderableFunction.callByValue(1))
    val c = Closure.createVarArgsClosure(Left("l"), Right(Closure[String, String](f, Left("Hello"))), Left("K"))
    c.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),scala.collection.Seq](1,  mkList(a?)), Right(Closure(RenderableFunction[List(java.lang.String),java.lang.String](1,  upper(a?)), Left(\"Hello\"))))"
    c.arity shouldBe 0
    c() shouldBe Success(Seq("l", "HELLO", "K"))
  }

  behavior of "Closure with lookup"
  it should "work for simple when variable specified at last moment" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    val sArg1 = "p1"
    val col1 = "col1"
    val val1 = "42"
    // XXX: create an empty set of variables
    val variables = mutable.HashMap[String, String]()
    implicit val lookup: String => Try[String] = { s => Spy.spy(s"lookup $s", FP.optionToTry(variables.get(s))) }

    def fStringToInt(arg1: String): Int = arg1.toInt

    val wFunction = FunctionString.custom("stringToInt", List(sArg1))
    val fFunction: RenderableFunction[Int] = Spy.spy("fFunction", RenderableFunction(fStringToInt _, wFunction, RenderableFunction.callByValue(1)))
    // NOTE: that we have to cast the renderable fourStringFunction because internally it always replaces Try[Try[R]] with Try[R]
    val fLookup = Spy.spy("fLookup", RenderableFunction(lookup, "lookup", Seq(false)).asInstanceOf[RenderableFunction[String]])
    val p1: Parameter[String] = Spy.spy("p1", Left(col1))
    val closureLookup1: Closure[String, String] = Spy.spy("closureLookup", Closure[String, String](fLookup, p1))
    closureLookup1.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"col1\"))"
    val p2: Either[String, Closure[String, String]] = Spy.spy("p2", Right(closureLookup1))
    val c: Closure[String, Int] = Spy.spy("c", Closure[String, Int](fFunction, p2))
    c.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),Int](1,  stringToInt(p1?)), Right(Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"col1\"))))"
    val dy = Spy.spy("dy", for (d <- c.partiallyApply) yield d)
    // XXX: provide the variable values at the last moment
    variables.put(col1, val1)
    (for (d <- dy; r <- d()) yield r) match {
      case Success(s) => s shouldBe 42
      case Failure(x) => fail(x)
    }
  }

  it should "work for fourStringFunction" in {
    case class Converter[R](f: Scalar => Option[R], name: String) extends (Scalar => Option[R]) {
      def apply(s: Scalar): Option[R] = f(s)

      override def toString: String = name
    }

    def mapLookup(m: => Map[String, String]): String => Try[String] = { s => FP.optionToTry(m.get(s)) }

    val function = "fourStringFunction"
    val sArg1 = "p1"
    val sArg2 = "p2"
    val sArg3 = "p3"
    val sArg4 = "p4"
    val col1 = "c1"
    val col2 = "c2"
    val val1 = "v1"
    val val2 = "v2"
    val val3 = "v3"
    val val4 = "v4"
    val variables = Map(col1 -> val1, col2 -> val2)
    implicit val lookup: String => Try[String] = mapLookup(variables)

    def fourStringFunction(arg1: String, arg2: String, arg3: String, arg4: String): String = s"$function($sArg1: $arg1, $sArg2: $arg2, $sArg3: $arg3, $sArg4: $arg4)"

    def concat(p1: String): String = p1

    val wFunction = FunctionString.custom(function, List(sArg1, sArg2, sArg3, sArg4))
    val fFunction: RenderableFunction[String] = RenderableFunction(fourStringFunction _, wFunction, RenderableFunction.callByValue(4))
    val wConcat = FunctionString.custom("concat", Seq("x"))
    val fConcat = RenderableFunction(concat _, wConcat, RenderableFunction.callByValue(1))
    // NOTE: that we have to cast the renderable fourStringFunction because internally it always replaces Try[Try[R]] with Try[R]
    val fLookup = RenderableFunction(lookup, "lookup", Seq(false)).asInstanceOf[RenderableFunction[String]]
    val p1 = Left(col1)
    val closureLookup1 = Closure[String, String](fLookup, p1)
    closureLookup1.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"c1\"))"
    val p3 = Right(closureLookup1)
    val p2 = Left(col2)
    val closureLookup2 = Closure[String, String](fLookup, p2)
    closureLookup2.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"c2\"))"
    val closureConcat = Closure[String, String](fConcat, p3)
    closureConcat.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),java.lang.String](1,  concat(x?)), Right(Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"c1\"))))"
    val p4 = Right(closureConcat)
    val p5 = Right(closureLookup2)
    val p6 = Left(val3)
    val p7 = Left(val4)
    val c = Closure[String, String](fFunction, p4, p5, p6, p7)
    c.render() shouldBe "Closure(RenderableFunction[List(java.lang.String, java.lang.String, java.lang.String, java.lang.String),java.lang.String](4,  fourStringFunction(p1?)(p2?)(p3?)(p4?)), \n  Right(Closure(RenderableFunction[List(java.lang.String),java.lang.String](1,  concat(x?)), Right(Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"c1\"))))),\n  Right(Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"c2\"))),\n  Left(\"v3\"),\n  Left(\"v4\")\n)"
    val dy: Try[Closure[_, String]] = for (d <- c partiallyApply) yield d
    dy match {
      case Success(x) => x.render() shouldBe "Closure(RenderableFunction[List(),java.lang.String](0,  fourStringFunction(concat(lookup(\"c1\")))(lookup(\"c2\"))(\"v3\")(\"v4\")), )"
      case _ => fail("invalid partial apply")
    }
    (for (d <- dy; r <- d()) yield r) match {
      case Success(s) => s shouldBe s"fourStringFunction($sArg1: $val1, $sArg2: $val2, $sArg3: $val3, $sArg4: $val4)"
      case Failure(x) => fail(x)
    }
  }

  it should "work for fourStringFunction when variables specified at last moment" in {
    implicit val logger: Logger = Spy.getLogger(getClass)
    case class Converter[R](f: Scalar => Option[R], name: String) extends (Scalar => Option[R]) {
      def apply(s: Scalar): Option[R] = f(s)

      override def toString: String = name
    }

    val function = "fourStringFunction"
    val sArg1 = "p1"
    val sArg2 = "p2"
    val sArg3 = "p3"
    val sArg4 = "p4"
    val col1 = "col1"
    val col2 = "col2"
    val val1 = "VALUE_1"
    val val2 = "VALUE_2"
    val val3 = "VALUE_3"
    val val4 = "VALUE_4"
    // XXX: create an empty set of variables
    val variables = mutable.HashMap[String, String]()
    implicit val lookup: String => Try[String] = { s => FP.optionToTry(variables.get(s)) }

    def fourStringFunction(arg1: String, arg2: String, arg3: String, arg4: String): String = s"$function($sArg1: $arg1, $sArg2: $arg2, $sArg3: $arg3, $sArg4: $arg4)"

    def concat(p1: String): String = p1

    val wFunction = FunctionString.custom(function, List(sArg1, sArg2, sArg3, sArg4))
    val fFunction: RenderableFunction[String] = Spy.spy("fFunction", RenderableFunction(fourStringFunction _, wFunction, RenderableFunction.callByValue(4)))
    val wConcat = Spy.spy("wConcat", FunctionString.custom("concat", Seq("x")))
    val fConcat = Spy.spy("fConcat", RenderableFunction(concat _, wConcat, RenderableFunction.callByValue(1)))
    // NOTE: that we have to cast the renderable fourStringFunction because internally it always replaces Try[Try[R]] with Try[R]
    val fLookup = Spy.spy("fLookup", RenderableFunction(lookup, "lookup", Seq(false)).asInstanceOf[RenderableFunction[String]])
    val p1 = Spy.spy("p1", Left(col1))
    val closureLookup1 = Spy.spy("closureLookup", Closure[String, String](fLookup, p1))
    val p3 = Spy.spy("p3", Right(closureLookup1))
    val p2 = Spy.spy("p2", Left(col2))
    val closureLookup2 = Spy.spy("closureLookup2", Closure[String, String](fLookup, p2))
    val closureConcat = Spy.spy("closureConcat", Closure[String, String](fConcat, p3))
    val p4 = Spy.spy("p4", Right(closureConcat))
    val p5 = Spy.spy("p5", Right(closureLookup2))
    val p6 = Spy.spy("p6", Left(val3))
    val p7 = Spy.spy("p7", Left(val4))
    val c = Spy.spy("c", Closure[String, String](fFunction, p4, p5, p6, p7))
    val dy = Spy.spy("dy", for (d <- c partiallyApply) yield d)
    // XXX: provide the variable values at the last moment
    variables.put(col1, val1)
    variables.put(col2, val2)
    (for (d <- dy; r <- d()) yield r) match {
      case Success(s) => s shouldBe s"fourStringFunction($sArg1: $val1, $sArg2: $val2, $sArg3: $val3, $sArg4: $val4)"
      case Failure(x) => fail(x)
    }
  }

  behavior of "hashCode"
  it should "match" in {
    val sArg1 = "p1"
    val col1 = "col1"
    // XXX: create an empty set of variables
    val variables = mutable.HashMap[String, String]()
    implicit val lookup: String => Try[String] = { s => FP.optionToTry(variables.get(s)) }

    def fStringToInt(arg1: String): Int = arg1.toInt

    val wFunction = FunctionString.custom("stringToInt", List(sArg1))
    val fFunction = RenderableFunction(fStringToInt _, wFunction, RenderableFunction.callByValue(1))
    // NOTE: that we have to cast the renderable fourStringFunction because internally it always replaces Try[Try[R]] with Try[R]
    val fLookup = RenderableFunction(lookup, "lookup", Seq(false)).asInstanceOf[RenderableFunction[String]]
    val p1 = Left(col1)
    val closureLookup1 = Closure[String, String](fLookup, p1)
    closureLookup1.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"col1\"))"
    val p2 = Right(closureLookup1)
    val c1 = Closure[String, Int](fFunction, p2)
    c1.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),Int](1,  stringToInt(p1?)), Right(Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"col1\"))))"
    val c2 = Closure[String, Int](fFunction, p2)
    c2.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),Int](1,  stringToInt(p1?)), Right(Closure(RenderableFunction[List(java.lang.String),scala.util.Try](1,  lookup(a?)), Left(\"col1\"))))"
    c1.hashCode() shouldBe c2.hashCode()
    (c1 == c2) shouldBe true
    val dy1 = for (d <- c1 partiallyApply) yield d
    val dy2 = for (d <- c2 partiallyApply) yield d
    dy1.get.hashCode() shouldBe dy2.get.hashCode()
  }

  behavior of "??"
  it should "handle apply" in {
    val x = ??(Seq("1"))
    x.get shouldBe Seq("1")
  }
  it should "handle +:" in {
    val x = ??(Seq("2"))
    val y = "1" +: x
    y.get shouldBe Seq("1", "2")
  }
  it should "handle ++:" in {
    val x = ??(Seq("2"))
    val y = Seq("1") ++: x
    y.get shouldBe Seq("1", "2")
  }
}
