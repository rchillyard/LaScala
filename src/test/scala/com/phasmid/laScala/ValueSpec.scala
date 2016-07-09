package com.phasmid.laScala

import org.scalatest.{FlatSpec, Matchers}


/**
  * @author scalaprof
  */
class ValueSpec extends FlatSpec with Matchers {
  "IntValue" should "work" in {
    val x = Value(1)
    x.source shouldBe 1
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  "StringValue" should "work where string is numeric" in {
    val x = Value("1")
    x.source shouldBe "1"
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work where string is not numeric" in {
    val x = Value("X")
    x.source shouldBe "X"
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  "DoubleValue" should "work" in {
    val x = Value(1.0)
    x.source shouldBe 1.0
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  "attribute map" should "work" in {
    val m: Map[String, Value[_]] = Map("k" -> Value("k"), "1" -> Value(1), "1.0" -> Value(1.0))
    val xos = for ((k, v) <- m) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 2
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
}
