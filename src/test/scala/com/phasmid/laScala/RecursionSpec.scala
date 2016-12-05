package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  *
  * @author scalaprof
  */
class RecursionSpec extends FlatSpec with Matchers with Inside {
  behavior of "Factorial.factorial"
  it should "yield appropriate values" in {
    Factorial.factorial(1) shouldBe BigInt(1)
    Factorial.factorial(2) shouldBe BigInt(2)
    Factorial.factorial(3) shouldBe BigInt(6)
    Factorial.factorial(10) shouldBe BigInt(3628800)
    Factorial.factorial(20) shouldBe BigInt(2432902008176640000L)
  }
}
