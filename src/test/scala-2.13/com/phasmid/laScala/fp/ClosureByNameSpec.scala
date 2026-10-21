/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.Scalar
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Left, _}

/**
  * These (ignored) tests use functions with call-by-name parameters, e.g. `def isHello(s: => String)`.
  * Only Scala 2 can infer a by-name type such as `=> String` for a type parameter, so they compile only for Scala 2.
  *
  * Created by scalaprof on 10/19/16.
  */
class ClosureByNameSpec extends AnyFlatSpec with Matchers {

  behavior of "Closure with call-by-name parameters"

  ignore should "apply for a simple call-by-name closure" in {
    val name = "isHello"

    def isHello(s: => String): Boolean = s == "Hello"

    val f = RenderableFunction(isHello _, name, RenderableFunction.callByName(1))
    val c = Closure(f, Left("Hello"))
    c.arity shouldBe 0
    c() shouldBe Success(true)
  }

  ignore should "apply for a closure where the call-by-name parameter is itself a Closure" in {
    //    val getPi: Product => String = { _: () => math.Pi.toString }
    def getPi: String = math.Pi.toString

    def isHello(s: => String): Boolean = s == "Hello"

    val f1 = RenderableFunction(isHello _, "isHello", RenderableFunction.callByName(1))
    val f2 = RenderableFunction(() => getPi, "pi", Seq[Boolean]())
    val c1 = Closure(f2)
    val c2 = Closure(f1, Right(c1))
    c2.arity shouldBe 0
    c2() shouldBe Success(false)
  }
}
