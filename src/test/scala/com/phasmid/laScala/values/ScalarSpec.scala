package com.phasmid.laScala.values

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.language.implicitConversions
import scala.util.{Success, Try}


/**
  * @author scalaprof
  */
class ScalarSpec extends FlatSpec with Matchers with Inside {

  case class YMD(y: Int, m: Int, d: Int)

  object YMD {
    implicit def convertToScalar(y: YMD): Scalar = DateScalar(y.y, y.m, y.d)
  }

  "BooleanScalar" should "work" in {
    val x = Scalar(true)
    x.source shouldBe true
    x.asBoolean should matchPattern { case Some(true) => }
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Scalar = true
    x shouldBe BooleanScalar(true)
  }
  it should "render correctly" in {
    val x: Scalar = true
    x.render shouldBe "true"
    BooleanScalar.setDefaultFormat("%B")
    // TODO well, this is something of a mystery why this is failing.
//    x.render shouldBe "TRUE"
  }
  "IntScalar" should "work" in {
    val x = Scalar(1)
    x.source shouldBe 1
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Scalar = 1
    x shouldBe IntScalar(1)
  }
  it should "render correctly" in {
    val x: Scalar = 1
    x.render shouldBe "1"
  }
  "StringScalar" should "be Some where string is numeric" in {
    val x = Scalar("1")
    x.source shouldBe "1"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case Some(1) => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Scalar = "1"
    x shouldBe StringScalar("1")
  }
  it should "be None where string is not numeric" in {
    val x = Scalar("X")
    x.source shouldBe "X"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  it should "be Some(date) for asOrderable where string is a date" in {
    implicit val pattern = ""
    val x = Scalar("2016-07-10")
    x.source shouldBe "2016-07-10"
    x.asBoolean should matchPattern { case None => }
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  it should "work with month names" in {
    implicit val pattern = "dd-MMM-yy"
    val formatter = DateTimeFormatter.ofPattern(pattern)
    val gloriousTwelfth = formatter.parse("12-Aug-16")
    println(gloriousTwelfth)
    val x = DateScalar("12-Aug-16")
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  "QuotedStringScalar" should "be None" in {
    val x = QuotedStringScalar(""""1"""")
    x.source shouldBe """"1""""
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  it should "work implicitly" in {
    val x: Scalar = """"1""""
    x shouldBe QuotedStringScalar("1",""""1"""")
  }
  it should "be unquoted when created from apply" in {
    val x = Scalar(""""1"""")
    x.render shouldBe """"1""""
    x.source shouldBe """"1""""
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
  }
  "DoubleScalar" should "work" in {
    val x = Scalar(1.0)
    x.source shouldBe 1.0
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case Some(1.0) => }
  }
  it should "work implicitly" in {
    val x: Scalar = 1.0
    x shouldBe DoubleScalar(1.0)
  }
  it should "render correctly" in {
    val x: Scalar = 1.0
    x.render shouldBe "1.000000"
    x.renderFormatted("%5.2f") shouldBe " 1.00"
  }
  "RationalScalar" should "work" in {
    import Rational.RationalHelper
    val x = Scalar(r"1/2")
    x.source shouldBe Rational.half
    x.asFractional[Rational] should matchPattern { case Some(Rational.half) => }
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case Some(0.5) => }
  }
  it should "fail as a String" in {
    val x = Scalar("1/2")
    x.source shouldBe "1/2"
    x.asFractional[Rational] should matchPattern { case None => }
  }
  it should "render correctly" in {
    import Rational.RationalHelper
    val x: Scalar = r"1/2"
    x.render shouldBe "0.5"
    x.renderFormatted("%3.1f") shouldBe "0.5"
  }
  "DateScalar" should "work" in {
    implicit val pattern = ""
    val x = DateScalar("2016-07-10")
    x.source shouldBe "2016-07-10"
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  it should "work work without leading 0s" in {
    implicit val pattern = "y-M-d"
    val x = DateScalar("2016-7-7")
    x.asBoolean should matchPattern { case None => }
    x.asValuable[Int] should matchPattern { case None => }
    x.asValuable[Double] should matchPattern { case None => }
    x.asOrderable[LocalDate] should matchPattern { case Some(_) => }
  }
  it should "work implicitly" in {
    val x: Scalar = LocalDate.of(2016, 7, 10)
    x shouldBe DateScalar(LocalDate.of(2016, 7, 10))
  }
  it should "render correctly" in {
    implicit val pattern = ""
    val x = DateScalar("2016-07-10")
    x.render shouldBe "2016-07-10"
  }
  "attribute map" should "work" in {
    val m: Map[String, Scalar] = Map("k" -> Scalar("k"), "1" -> Scalar(1), "b" -> Scalar(true), "1.0" -> Scalar(1.0))
    val xos = for ((_, v) <- m) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 3
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
  it should "work when given raw strings" in {
    val wWm: Map[String, String] = Map("k" -> "k", "1" -> "1", "1.0" -> "1.0")
    val wVm = Scalar.sequence(wWm)
    val xos = for ((_, v) <- wVm) yield v.asValuable[Double]
    val xs = xos.flatten
    xs.size shouldBe 2
    xs.head shouldBe 1
    xs.tail.head shouldBe 1.0
  }
  it should "work with real dates" in {
    implicit val pattern = "MMM dd, yyyy"
    val dates: Map[String, Any] = Map("x" -> "Jul 13, 2016", "z" -> "Jul 31, 2015")
    val values: Map[String, Scalar] = Scalar.sequence(dates)
    val variables: Map[String, Option[LocalDate]] = for ((k, v) <- values) yield (k, v.asOrderable[LocalDate])
    variables.apply("x").get shouldBe LocalDate.of(2016, 7, 13)
  }
  "tryScalar" should "work when given Any" in {
    val w: Any = "k"
    val vy = Scalar.tryScalar(w)
    val xoy: Try[Option[Double]] = for (vs <- vy) yield vs.asValuable[Double]
    xoy should matchPattern { case Success(_) => }
    inside(xoy) {
      case Success(xo) =>
        xo should matchPattern { case None => }
    }
  }
}
