/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala.fp

import com.phasmidsoftware.lascala.fp.RenderableFunction.getPartialApplicationFunction
import com.phasmidsoftware.lascala.values.{BooleanScalar, Scalar, StringScalar, Tuple0}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Left, _}

/**
  * These (ignored) tests use functions with call-by-name parameters, e.g. `def isHello(s: => String)`.
  * Only Scala 2 can infer a by-name type such as `=> String` for a type parameter, so they compile only for Scala 2.
  *
  * Created by scalaprof on 10/19/16.
  */
class RenderableFunctionByNameSpec extends AnyFlatSpec with Matchers with PrivateMethodTester {

  behavior of "RenderableFunction with call-by-name parameters"

  ignore should "work with partiallyApply (2)" in {
    val name = "and"

    def and(x: Boolean, y: => Boolean) = x && y

    val f = RenderableFunction(and _, name, List(false, true))
    val bt: Boolean = true
    val bf: Boolean = false
    val g1y: Try[RenderableFunction[Boolean]] = f.partiallyApply(bt)
    val g2y: Try[RenderableFunction[Boolean]] = for (g <- g1y; h <- g.partiallyApply(bf)) yield h
    val by: Try[Boolean] = for (g2 <- g2y; b <- g2.callByName()) yield b
    by should matchPattern { case Success(false) => }
  }

  ignore should "work with partiallyApply (3)" in {
    val name = "and"

    def and(x: String, y: => String) = x.toBoolean && y.toBoolean

    val f = RenderableFunction(and _, name, List(false, true))
    val g1y: Try[RenderableFunction[Boolean]] = f.partiallyApply("true")
    val g2y: Try[RenderableFunction[Boolean]] = for (g <- g1y; h <- g.partiallyApply("false")) yield h
    val by: Try[Boolean] = for (g2 <- g2y; b <- g2.callByName()) yield b
    by should matchPattern { case Success(false) => }
  }

  ignore should "work with partiallyApply (4)" in {
    val name = "and"

    def and(x: String, y: => String) = (x.toBoolean && y.toBoolean).toString

    val f = RenderableFunction(and _, name, List(false, true))
    val g1y: Try[RenderableFunction[String]] = f.partiallyApply("true")
    val g2y: Try[RenderableFunction[String]] = for (g <- g1y; h <- g.partiallyApply("false")) yield h
    val by: Try[String] = for (g2 <- g2y; b <- g2.callByName()) yield b
    by should matchPattern { case Success("false") => }
  }

  ignore should "handle a function (1)" in {
    // First we give a bad value of pi
    val map = mutable.HashMap("pi" -> "22/7")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val c1 = Closure(lookup, Left("pi"))
    c1.render() shouldBe "Closure(RenderableFunction[List(scala.Function0),java.lang.String](1, => lookup(=>a?)), Left(\"pi\"))"
    val c2 = Closure(f, Right(c1))
    c2.render() shouldBe "Closure(RenderableFunction[List(java.lang.String),java.lang.String](1,  render(a?)), Right(Closure(RenderableFunction[List(scala.Function0),java.lang.String](1, => lookup(=>a?)), Left(\"pi\"))))"
    val c3y = c2.partiallyApply
    c3y should matchPattern { case Success(_) => }
    c3y.get.arity shouldBe 0
    c3y.get.render() shouldBe "Closure(RenderableFunction[List(),java.lang.String](0,  render(lookup(\"pi\"))), )"


    // At this point, c4y does not "close" over the value of pi, it is still to be evaluated which means that we can set the new, proper value, before we apply c4y.get
    map.put("pi", "3.1415927")
    val wy = for (c3 <- c3y; c4 <- c3()) yield c4
    wy shouldBe Success("3.1415927")
  }

  ignore should "handle a function (2)" in {
    val map = Map("pi" -> "3.1415927")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1.toDouble

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val c1 = Closure(lookup, Left("pi"))
    c1.render() shouldBe "Closure(RenderableFunction[List(scala.Function0),java.lang.String](1, => lookup(=>a?)), Left(\"pi\"))"
    val c2 = Closure(f, Right(c1))
    val xfy: Try[Closure[_, Double]] = c2.partiallyApply
    xfy should matchPattern { case Success(_) => }
    xfy.get.arity shouldBe 0
    xfy.get() shouldBe Success(3.1415927)
  }

  ignore should "handle a function that throws an exception (1)" in {
    val map = Map("PI" -> "3.1415927")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val fy: Try[RenderableFunction[String]] = f.partiallyApplyParameters(Seq(Right(Closure(lookup, Left("pi")))).asInstanceOf[Seq[Either[_, Closure[_, _]]]])
    val ry = for (g: RenderableFunction[String] <- fy; r <- g.callByName()) yield r
    ry should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  ignore should "handle a function that throws an exception 2)" in {
    val map = Map("PI" -> "3.1415927")

    def fLookup(s: => String): String = map(s)

    val lookup: RenderableFunction[String] = RenderableFunction(fLookup _, "lookup", Seq(true))

    def show(s1: String) = s1

    val f = RenderableFunction(show _, "render", RenderableFunction.callByValue(1))
    val cy = f.partiallyApplyParameters(List[Parameter[String]](Right(Closure(lookup, Left("pi")))).asInstanceOf[Seq[Either[_, Closure[_, _]]]])
    cy should matchPattern { case Failure(_: RenderableFunctionException) => }
  }

  ignore should """evaluate to true with true & true""" in {
    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", List(false, true))
    val cAnd = Closure(ifAnd, Left(true.booleanValue), Left(true.booleanValue))
    cAnd() match {
      case Success(b) => println(s"result: $b"); // Succeeded
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  ignore should """evaluate to true with true & (java) true""" in {
    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", List(false, true))
    val cAnd = Closure(ifAnd, Left(true.booleanValue), Left(true))
    cAnd() match {
      case Success(b) => println(s"result: $b"); // Succeeded
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  ignore should """evaluate (using java Boolean parameter) to true with true & (java) true""" in {
    def fAnd(p1: Boolean, p2: => java.lang.Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", List(false, true))
    val cAnd = Closure(ifAnd, Left(true), Left(true))
    cAnd() match {
      case Success(b) => println(s"result: $b"); // Succeeded
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  ignore should """evaluate to java.lang.Boolean(true) with true & (java) true""" in {
    def fAnd(p1: java.lang.Boolean, p2: => java.lang.Boolean): java.lang.Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", List(false, true))
    val cAnd = Closure(ifAnd, Left(true), Left(true))
    cAnd() match {
      case Success(b) => println(s"result: $b"); // Succeeded
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  ignore should """evaluate to java.lang.Boolean(true) with true & not (java) true""" in {
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}

    val ifNot = RenderableFunction(fNot _, "not", RenderableFunction.callByValue(1))

    def fAnd(p1: java.lang.Boolean, p2: => java.lang.Boolean): java.lang.Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", List(false, true))
    val cAnd = Closure(ifAnd, Left(true), Right(Closure(ifNot, Left(false))))
    println(cAnd)
    val g = cAnd.partiallyApply
    println(s"g=$g")
    cAnd() match {
      case Success(b) => println(s"result: $b"); // Succeeded
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  ignore should """evaluate to true with true & not true""" in {
    def fNot(p1: Boolean): Boolean = {println(s"call NOT $p1"); !p1}

    val ifNot = RenderableFunction(fNot _, "not", RenderableFunction.callByValue(1))

    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2

    val ifAnd = RenderableFunction(fAnd _, "and", List(false, true))
    val cAnd = Closure(ifAnd, Left(true), Right(Closure(ifNot, Left(false))))
    println(cAnd)
    val g = cAnd.partiallyApply
    println(s"g=$g")
    cAnd() match {
      case Success(b) => println(s"result: $b"); // Succeeded
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }
}
