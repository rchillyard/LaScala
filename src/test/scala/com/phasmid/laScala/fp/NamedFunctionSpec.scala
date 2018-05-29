/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.parser.FunctionStringParser
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class NamedFunctionSpec extends FlatSpec with Matchers {

  behavior of "NamedFunction"
  it should "yield correct name" in {
    val f: NamedFunction[Int, Int] = NamedFunction("test", identity)
    f.toString shouldBe "<function1: test>"
  }
  it should "apply correctly" in {
    def double(x: Int): Int = 2 * x

    val f: NamedFunction[Int, Int] = NamedFunction("double", double)
    f(1) shouldBe 2
  }
  it should "unapply correctly" in {
    def double(x: Int): Int = 2 * x

    val f = NamedFunction[Int, Int]("double", double)
    f match {
      case NamedFunction("double", g) => g(2) shouldBe 4
      case _ => fail
    }
  }
  it should "compose with fDouble" in {
    val p = CompoundFunctionStringParser("<function1: ", "&&&", ">")
    def fDouble(x: Int): Int = 2 * x

    val sfDouble = "fDouble"
    val f = NamedFunction(sfDouble, fDouble)
    val g = f.compose(fDouble)
    g(1) shouldBe 4
    p.parseCompoundFunctionString(g.toString) should matchPattern {
      case Success((`sfDouble`, List("com.phasmid.laScala.fp.NamedFunctionSpec$$"), _)) => // Scala 2.12
      case Success((`sfDouble`, List("function1"), _)) => // Scala 2.10, 2.11
    }
  }
  it should "compose with itself" in {
    def fDouble(x: Int): Int = 2 * x

    val f = NamedFunction("fDouble", fDouble)
    val g = f.compose(f)
    g(1) shouldBe 4
    g.toString shouldBe "<function1: fDouble&&&fDouble>"
  }

  behavior of "NamedFunction0"
  it should "yield correct name" in {
    def x(): Int = 1

    val f: NamedFunction0[Int] = NamedFunction0[Int]("test", x)
    f.toString shouldBe "<function0: test>"
  }
  it should "apply correctly" in {
    def x(): Int = 1

    val f: NamedFunction0[Int] = NamedFunction0("test", x)
    f() shouldBe 1
  }
  it should "unapply correctly" in {
    def x(): Int = 1

    val f: NamedFunction0[Int] = NamedFunction0("test", x)
    f match {
      case NamedFunction0("test", g) => g() shouldBe 1
      case _ => fail
    }
  }

  behavior of "NamedFunction2"
  it should "yield correct name" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = NamedFunction2("test", x)
    f.toString shouldBe "<function2: test>"
  }
  it should "apply correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = NamedFunction2("test", x)
    f(2, 3) shouldBe 8.0 +- 0.001
  }
  it should "unapply correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = NamedFunction2("test", x)
    f match {
      case NamedFunction2("test", g) => g(2, 3) shouldBe 8.0 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = NamedFunction2("test", x)
    val g = f.curried
    g.toString shouldBe "<function1: test!!!>"
    g match {
        // NOTE: do not follow recommendation to use syntactic sugar here and in similar places
      case _: Function1[_, _] =>
      case _ => fail
    }
    g(2)(3) shouldBe 8.0 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test!!!"
        h(2)(3) shouldBe 8.0 +- 0.001
      case _ => fail
    }
  }
  it should "tuple correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = NamedFunction2("test", x)
    val g = f.tupled
    g.toString shouldBe "<function1: test###>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g((2, 3)) shouldBe 8.0 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test###"
        h((2, 3)) shouldBe 8.0 +- 0.001
      case _ => fail
    }
  }

  behavior of "NamedFunction3"
  it should "yield correct name" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = NamedFunction3("test", x)
    f.toString shouldBe "<function3: test>"
  }
  it should "apply correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = NamedFunction3("test", x)
    f("10", 2, 3) shouldBe 80.0 +- 0.001
  }
  it should "unapply correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = NamedFunction3("test", x)
    f match {
      case NamedFunction3("test", g) => g("10", 2, 3) shouldBe 80.0 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = NamedFunction3("test", x)
    val g = f.curried
    g.toString shouldBe "<function1: test!!!>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g("10")(2)(3) shouldBe 80.0 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test!!!"
        h("10")(2)(3) shouldBe 80.0 +- 0.001
      case _ => fail
    }
  }
  it should "tuple correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = NamedFunction3("test", x)
    val g = f.tupled
    g.toString shouldBe "<function1: test###>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g(("10", 2, 3)) shouldBe 80.0 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test###"
        h(("10", 2, 3)) shouldBe 80.0 +- 0.001
      case _ => fail
    }
  }

  behavior of "NamedFunction4"
  it should "yield correct name" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = NamedFunction4("test", x)
    f.toString shouldBe "<function4: test>"
  }
  it should "apply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = NamedFunction4("test", x)
    f("10", 2, 3, Math.PI) shouldBe 251.327 +- 0.001
  }
  it should "unapply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = NamedFunction4("test", x)
    f match {
      case NamedFunction4("test", g) => g("10", 2, 3, Math.PI) shouldBe 251.327 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = NamedFunction4("test", x)
    val g = f.curried
    g.toString shouldBe "<function1: test!!!>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g("10")(2)(3)(Math.PI) shouldBe 251.327 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test!!!"
        h("10")(2)(3)(Math.PI) shouldBe 251.327 +- 0.001
      case _ => fail
    }
  }
  it should "tuple correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = NamedFunction4("test", x)
    val g = f.tupled
    g.toString shouldBe "<function1: test###>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g(("10", 2, 3, Math.PI)) shouldBe 251.327 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test###"
        h(("10", 2, 3, Math.PI)) shouldBe 251.327 +- 0.001
      case _ => fail
    }
  }

  behavior of "NamedFunction5"
  it should "yield correct name" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = NamedFunction5("test", x)
    f.toString shouldBe "<function5: test>"
  }
  it should "apply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = NamedFunction5("test", x)
    f("10", 2, 3, Math.PI, false) shouldBe 0.0 +- 0.001
  }
  it should "unapply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = NamedFunction5("test", x)
    f match {
      case NamedFunction5("test", g) => g("10", 2, 3, Math.PI, false) shouldBe 0.0 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = NamedFunction5("test", x)
    val g = f.curried
    g.toString shouldBe "<function1: test!!!>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g("10")(2)(3)(Math.PI)(false) shouldBe 0.0 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test!!!"
        h("10")(2)(3)(Math.PI)(false) shouldBe 00.0 +- 0.001
      case _ => fail
    }
  }
  it should "tuple correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = NamedFunction5("test", x)
    val g = f.tupled
    g.toString shouldBe "<function1: test###>"
    g match {
      case _: Function1[_, _] =>
      case _ => fail
    }
    g(("10", 2, 3, Math.PI, false)) shouldBe 0.0 +- 0.001
    g match {
      case NamedFunction(w, h) =>
        w shouldBe "test###"
        h(("10", 2, 3, Math.PI, false)) shouldBe 0.0 +- 0.001
      case _ => fail
    }
  }
}

case class CompoundFunctionStringParser(pp: String, ps: String, s: String) extends FunctionStringParser {
  def prefix: Parser[String] = pp ~> """\w+""".r <~ ps

  def suffix: Parser[String] = s ~> ""
}
