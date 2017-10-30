/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}

class NamedFunctionSpec extends FlatSpec with Matchers {

  behavior of "NamedFunction"
  it should "yield correct name" in {
    val f: NamedFunction[Int, Int] = new NamedFunction("test", identity)
    f.toString shouldBe "<function1: test>"
  }
  it should "apply correctly" in {
    def double(x: Int): Int = 2 * x

    val f: NamedFunction[Int, Int] = new NamedFunction("double", double)
    f(1) shouldBe 2
  }
  it should "unapply correctly" in {
    def double(x: Int): Int = 2 * x

    val f: NamedFunction[Int, Int] = new NamedFunction("double", double)
    f match {
      case NamedFunction("double", g) => g(2) shouldBe 4
      case _ => fail
    }
  }
  it should "compose with fDouble" in {
    def fDouble(x: Int): Int = 2 * x

    val f = new NamedFunction("fDouble", fDouble)
    val g = f.compose(fDouble)
    g(1) shouldBe 4
    g.toString shouldBe "<function1: fDouble&&&<function1>>"
  }
  it should "compose with itself" in {
    def fDouble(x: Int): Int = 2 * x

    val f = new NamedFunction("fDouble", fDouble)
    val g = f.compose(f)
    g(1) shouldBe 4
    g.toString shouldBe "<function1: fDouble&&&fDouble>"
  }

  behavior of "NamedFunction0"
  it should "yield correct name" in {
    def x(): Int = 1

    val f: NamedFunction0[Int] = new NamedFunction0("test", x)
    f.toString shouldBe "<function0: test>"
  }
  it should "apply correctly" in {
    def x(): Int = 1

    val f: NamedFunction0[Int] = new NamedFunction0("test", x)
    f() shouldBe 1
  }
  it should "unapply correctly" in {
    def x(): Int = 1

    val f: NamedFunction0[Int] = new NamedFunction0("test", x)
    f match {
      case NamedFunction0("test", g) => g() shouldBe 1
      case _ => fail
    }
  }

  behavior of "NamedFunction2"
  it should "yield correct name" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = new NamedFunction2("test", x)
    f.toString shouldBe "<function2: test>"
  }
  it should "apply correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = new NamedFunction2("test", x)
    f(2, 3) shouldBe 8.0 +- 0.001
  }
  it should "unapply correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = new NamedFunction2("test", x)
    f match {
      case NamedFunction2("test", g) => g(2, 3) shouldBe 8.0 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(v: Int, y: Int): Double = Math.pow(v, y)

    val f: NamedFunction2[Int, Int, Double] = new NamedFunction2("test", x)
    val g = f.curried
    g.toString shouldBe "<function1: test!!!>"
    g match {
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

    val f: NamedFunction2[Int, Int, Double] = new NamedFunction2("test", x)
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

    val f = new NamedFunction3("test", x)
    f.toString shouldBe "<function3: test>"
  }
  it should "apply correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = new NamedFunction3("test", x)
    f("10", 2, 3) shouldBe 80.0 +- 0.001
  }
  it should "unapply correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = new NamedFunction3("test", x)
    f match {
      case NamedFunction3("test", g) => g("10", 2, 3) shouldBe 80.0 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(s: String, v: Int, y: Int): Double = s.toDouble * Math.pow(v, y)

    val f = new NamedFunction3("test", x)
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

    val f = new NamedFunction3("test", x)
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

    val f = new NamedFunction4("test", x)
    f.toString shouldBe "<function4: test>"
  }
  it should "apply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = new NamedFunction4("test", x)
    f("10", 2, 3, Math.PI) shouldBe 251.327 +- 0.001
  }
  it should "unapply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = new NamedFunction4("test", x)
    f match {
      case NamedFunction4("test", g) => g("10", 2, 3, Math.PI) shouldBe 251.327 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(s: String, v: Int, y: Int, z: Double): Double = s.toDouble * Math.pow(v, y) * z

    val f = new NamedFunction4("test", x)
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

    val f = new NamedFunction4("test", x)
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

    val f = new NamedFunction5("test", x)
    f.toString shouldBe "<function5: test>"
  }
  it should "apply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = new NamedFunction5("test", x)
    f("10", 2, 3, Math.PI, false) shouldBe 0.0 +- 0.001
  }
  it should "unapply correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = new NamedFunction5("test", x)
    f match {
      case NamedFunction5("test", g) => g("10", 2, 3, Math.PI, false) shouldBe 0.0 +- 0.001
      case _ => fail
    }
  }
  it should "curry correctly" in {
    def x(s: String, v: Int, y: Int, z: Double, q: Boolean): Double = if (q) s.toDouble * Math.pow(v, y) * z else 0

    val f = new NamedFunction5("test", x)
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

    val f = new NamedFunction5("test", x)
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
