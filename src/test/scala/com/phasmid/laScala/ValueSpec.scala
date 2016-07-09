package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.{Success, Try}


/**
  * @author scalaprof
  */
class ValueSpec extends FlatSpec with Matchers with Inside {
  "IntValue" should "work" in {
    val x = Value(1)
    x.source shouldBe 1
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  "StringValue" should "be Some where string is numeric" in {
    val x = Value("1")
    x.source shouldBe "1"
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "be None where string is not numeric" in {
    val x = Value("X")
    x.source shouldBe "X"
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  "QuotedStringValue" should "be None" in {
    val x = QuotedStringValue(""""1"""")
    x.source shouldBe """"1""""
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  it should "be unquoted when created from apply" in {
    val x = Value(""""1"""")
    x.source shouldBe "1"
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
  it should "work when given raw strings" in {
    val wWm: Map[String, String] = Map("k" -> "k", "1" -> "1", "1.0" -> "1.0")
    val wVm = Value.sequence(wWm)
    val xos = for ((k, v) <- wVm) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 2
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
  "sequence" should "work when given raw strings" in {
    val ws = List("k", "1", "1.0")
    val vs = Value.sequence(ws)
    val xos = for (v <- vs) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 2
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
  "tryValue" should "work when given Any" in {
    val w: Any = "k"
    val vy = Value.tryValue(w)
    val xoy: Try[Option[Double]] = for (vs <- vy) yield vs.asValuable[Double]
    xoy should matchPattern { case Success(xos) => }
    inside (xoy) {
      case Success(xo) =>
        xo should matchPattern { case None => }
    }
  }
  "trySequence" should "work when given Anys" in {
    val ws = List[Any]("k", 1, 1.0)
    val vsy = Value.trySequence(ws)
    val xosy: Try[Seq[Option[Double]]] = for (vs <- vsy) yield for (v <- vs) yield v.asValuable[Double]
    xosy should matchPattern { case Success(xos) => }
    inside (xosy) {
      case Success(xos) =>
        xos should matchPattern { case List(None, Some(1.0), Some(1.0)) => }
    }
  }

}
