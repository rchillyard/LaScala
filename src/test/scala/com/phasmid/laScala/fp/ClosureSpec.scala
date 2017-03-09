/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.{BooleanScalar, Scalar, StringScalar}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scala.util.{Left, _}

/**
  * Created by scalaprof on 10/19/16.
  */
class ClosureSpec extends FlatSpec with Matchers {

  behavior of "Closure"
  it should "apply for a simple closure" in {
    val name = "isHello"
    val f = RenderableFunction({ s: String => s == "Hello" }, name, RenderableFunction.callByValue(1))
    val c = Closure(f, Left("Hello"))
    c.arity shouldBe 0
    c() shouldBe Success(true)
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

  behavior of "Closure with lookup"
  it should "work for fourStringFunction" in {
    case class Converter[R](f: Scalar => Option[R], name: String) extends (Scalar => Option[R]) {
      def apply(s: Scalar): Option[R] = f.apply(s)

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
    val fLookup = RenderableFunction(lookup, "lookup", Seq(true)).asInstanceOf[RenderableFunction[String]]
    fLookup.toString shouldBe "RenderableFunction(1, => lookup(=>a?))"
    val p1 = Left(col1)
    val closureLookup1 = Closure[String, String](fLookup, p1)
    val p3 = Right(closureLookup1)
    val p2 = Left(col2)
    val closureLookup2 = Closure[String, String](fLookup, p2)
    val closureConcat = Closure[String, String](fConcat, p3)
    val p4 = Right(closureConcat)
    val p5 = Right(closureLookup2)
    val p6 = Left(val3)
    val p7 = Left(val4)
    val c = Closure[String, String](fFunction, p4, p5, p6, p7)
    val dy = for (d <- c.partiallyApply()) yield d
    for (d <- dy) yield d.apply() match {
      case Success(s) => s shouldBe "key: " + val1 + ", " + sArg2 + ": " + val2
      case Failure(x) => fail(x)
    }
  }


}
