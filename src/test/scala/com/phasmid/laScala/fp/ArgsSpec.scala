package com.phasmid.laScala.fp

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 9/7/16.
  */
class ArgsSpec extends FlatSpec with Matchers {
    behavior of "Args"

    it should "get arguments from a variable argument list" in {
      def mainMethod(args: List[Any]): Unit = {
        val a1 = Args(args)
        val (x: String, a2) = a1.get(classOf[String]).get
        val (y, a3) = a2.get(classOf[java.lang.Boolean]).get
        a3.isEmpty shouldBe true
        x shouldBe "hello"
        y shouldBe true
      }
      mainMethod(List("hello", true))
  }
}
