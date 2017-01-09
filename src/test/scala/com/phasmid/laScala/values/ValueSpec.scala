package com.phasmid.laScala.values

import java.time.LocalDate

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.language.implicitConversions
import scala.util.{Success, Try}


/**
  * @author scalaprof
  */
class ValueSpec extends FlatSpec with Matchers with Inside {

  case class YMD(y: Int, m: Int, d: Int)

  object YMD {
    implicit def convertToValue(y: YMD): Value = DateValue(y.y, y.m, y.d)
  }

  "BooleanValue" should "work" in {
    val x = Value(true)
    x.source shouldBe true
    x.asBoolean should matchPattern { case Some(true) => }
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Value = true
    x shouldBe BooleanValue(true)
  }
  "IntValue" should "work" in {
    val x = Value(1)
    x.source shouldBe 1
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Value = 1
    x shouldBe IntValue(1)
  }
  "StringValue" should "be Some where string is numeric" in {
    val x = Value("1")
    x.source shouldBe "1"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Value = "1"
    x shouldBe StringValue("1")
  }
  it should "be None where string is not numeric" in {
    val x = Value("X")
    x.source shouldBe "X"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  it should "be Some(date) for asOrderable where string is a date" in {
    implicit val pattern = ""
    val x = Value("2016-07-10")
    x.source shouldBe "2016-07-10"
    x.asBoolean should matchPattern { case None => }
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  "QuotedStringValue" should "be None" in {
    val x = QuotedStringValue(""""1"""")
    x.source shouldBe """"1""""
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  it should "work implicitly" in {
    val x: Value = """"1""""
    x shouldBe QuotedStringValue("1",""""1"""")
  }
  it should "be unquoted when created from apply" in {
    val x = Value(""""1"""")
    x.render() shouldBe """"1""""
    x.source shouldBe """"1""""
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  "DoubleValue" should "work" in {
    val x = Value(1.0)
    x.source shouldBe 1.0
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Value = 1.0
    x shouldBe DoubleValue(1.0)
  }
  "RationalValue" should "work" in {
    import Rational.RationalHelper
    import FiniteIntegral.LongIsFiniteIntegral
    val x = Value(r"1/2")
    val half = Rational.half
    x.source shouldBe half
    x.asFractional[LongRational] should matchPattern { case Some(`half`) => }
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case Some(0.5) => }
  }
  it should "fail as a String" in {
    val x = Value("1/2")
    x.source shouldBe "1/2"
    x.asFractional[LongRational] should matchPattern { case None => }
  }
  it should "work implicitly" in {
    val x: Value = 1.0
    x shouldBe DoubleValue(1.0)
  }
  "DateValue" should "work out of the box" in {
    implicit val pattern = ""
    val x = DateValue("2016-07-10")
    x.source shouldBe "2016-07-10"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  it should "work with single digits given appropriate pattern" in {
    implicit val pattern = "y-M-d"
    val x = DateValue("2016-7-10")
    x.source shouldBe "2016-7-10"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  it should "work implicitly" in {
    val x: Value = LocalDate.of(2016, 7, 10)
    x shouldBe DateValue(LocalDate.of(2016, 7, 10))
  }
  "SequenceValue" should "work" in {
    val xs = Seq("2016-07-10", 1, """Hello""")
    implicit val pattern = ""
    val x: SequenceValue = SequenceValue(xs)
    x.source shouldBe xs
    x.asBoolean should matchPattern { case None => }
    for (vs <- x.asSequence) yield vs.size shouldBe 3
  }
  it should "work with embedded sequence" in {
    val xs = Seq("2016-07-10", 1, """Hello""", List(1, 2))
    implicit val pattern = ""
    val x: SequenceValue = SequenceValue(xs)
    x.source shouldBe xs
    x.asBoolean should matchPattern { case None => }
    for (vs <- x.asSequence) yield vs.size shouldBe 4
  }
  it should "work with sequence of YMD" in {
    implicit val valueMaker: ValueMaker = new ValueMaker {
      def value(x: Any): Try[Value] = x match {
        case y: YMD => Success(y)
        case _ => Value.tryValue(x)
      }
    }
    val xs = List(YMD(2016, 7, 15), YMD(2016, 7, 22), YMD(2016, 7, 29), YMD(2016, 8, 5), YMD(2016, 8, 12), YMD(2016, 8, 19), YMD(2016, 8, 26), YMD(2016, 9, 16), YMD(2016, 10, 21), YMD(2017, 1, 20), YMD(2017, 3, 17), YMD(2017, 4, 21), YMD(2017, 6, 16), YMD(2018, 1, 19))
    implicit val pattern = ""
    val x = Value.sequence(xs)
    for (vs <- x.asSequence) yield vs.size shouldBe 14
  }
  "attribute map" should "work" in {
    val m: Map[String, Value] = Map("k" -> Value("k"), "1" -> Value(1), "b" -> Value(true), "1.0" -> Value(1.0))
    val xos = for ((_, v) <- m) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 3
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
  it should "work when given raw strings" in {
    val wWm: Map[String, String] = Map("k" -> "k", "1" -> "1", "1.0" -> "1.0")
    val wVm = Value.sequence(wWm)
    val xos = for ((_, v) <- wVm) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 2
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
  it should "work with sequence of YMD" in {
    implicit val valueMaker: ValueMaker = new ValueMaker {
      def value(x: Any): Try[Value] = x match {
        case y: YMD => Success(y)
        case _ => Value.tryValue(x)
      }
    }
    val xs = List(YMD(2016, 7, 15), YMD(2016, 7, 22), YMD(2016, 7, 29), YMD(2016, 8, 5), YMD(2016, 8, 12), YMD(2016, 8, 19), YMD(2016, 8, 26), YMD(2016, 9, 16), YMD(2016, 10, 21), YMD(2017, 1, 20), YMD(2017, 3, 17), YMD(2017, 4, 21), YMD(2017, 6, 16), YMD(2018, 1, 19))
    val wWm = Map("expirations" -> xs)
    implicit val pattern = ""
    val x = Value.sequence(wWm)
    for (vs <- x.values; v <- vs.asSequence) yield v.size shouldBe 14
  }
  it should "work with real dates" in {
    implicit val pattern = "MMM dd, yyyy"
    val dates: Map[String, Any] = Map("x" -> "Jul 13, 2016", "z" -> "Jul 31, 2015")
    val values: Map[String, Value] = Value.sequence(dates)
    val variables: Map[String, Option[LocalDate]] = for ((k, v) <- values) yield (k, v.asOrderable[LocalDate])
    variables.apply("x").get shouldBe LocalDate.of(2016, 7, 13)
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
    xoy should matchPattern { case Success(_) => }
    inside(xoy) {
      case Success(xo) =>
        xo should matchPattern { case None => }
    }
  }
  "trySequence" should "work when given a list of Any" in {
    val ws = List[Any]("k", 1, 1.0)
    val vsy = Value.trySequence(ws)
    val xosy: Try[Seq[Option[Double]]] = for (vs <- vsy) yield for (v <- vs) yield v.asValuable[Double]
    xosy should matchPattern { case Success(_) => }
    inside(xosy) {
      case Success(xos) =>
        xos should matchPattern { case List(None, Some(1.0), Some(1.0)) => }
    }
  }
}
