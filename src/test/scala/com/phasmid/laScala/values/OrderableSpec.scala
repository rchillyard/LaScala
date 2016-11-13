package com.phasmid.laScala.values

import java.time.LocalDate

import com.phasmid.laScala.values.Orderable.{OrderableLocalDate, OrderableString}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps
import scala.util.Success

/**
  * @author scalaprof
  */
class OrderableSpec extends FlatSpec with Matchers {
  "1" should "result in 1" in {
    implicit val pattern = ""
    val orderable = implicitly[Orderable[Int]]
    val xt = orderable.fromString("1")
    xt should matchPattern { case Success(1) => }
    xt.get.compare(1) shouldBe 0
  }
  "1L" should "result in 1L" in {
    implicit val pattern = ""
    val orderable = implicitly[Orderable[Long]]
    val xt = orderable.fromString("1")
    xt should matchPattern { case Success(1L) => }
    xt.get.compare(1L) shouldBe 0
  }
  "1/2" should "result in 1/2" in {
    implicit val pattern = ""
    val orderable = implicitly[Orderable[Rational]]
    val xt = orderable.fromString("1/2")
    xt should matchPattern { case Success(Rational(1, 2)) => }
    xt.get.compare(1) shouldBe -1
  }
  "0.5" should "result in 0.5" in {
    implicit val pattern = ""
    val orderable = implicitly[Orderable[Double]]
    val xt = orderable.fromString("0.5")
    xt should matchPattern { case Success(0.5) => }
    xt.get.compare(1) shouldBe -1
  }
  "2016-01-01" should "result in OrderableString" in {
    implicit val pattern = ""
    val dt = OrderableString.unit("2016-01-01")
    dt shouldBe "2016-01-01"
  }
  "zero string" should "result in OrderableString of length zero" in {
    implicit val pattern = ""
    val dt = OrderableString.zero
  }
  "string1" should "be the string string1" in {
    OrderableString compare("string", "string") shouldBe 0
  }
  "string" should "be the string from string" in {
    implicit val pattern = ""
    val st = OrderableString.fromString("String1")
    st should matchPattern { case Success("String1") => }
  }
  "01/01/2016" should "result in string using via" in {
    val m: Map[String, String] = Map("k" -> "v")
    implicit val pattern = "MM/dd/uuuu"
    val st = OrderableString.viaLookup("k", m get)
    st should matchPattern { case Success("v") => }
  }
  // TODO we should move and modify the date-specific tests to IncrementableSpec
  "2016-01-01" should "result in OrderableDate" in {
    implicit val pattern = ""
    val dt = LocalDate of(2016, 2, 1)
    val dt2 = OrderableLocalDate.unit(dt)
    dt2 shouldBe dt
  }
  "2016-01-01" should "result in date" in {
    implicit val pattern = ""
    val dt = OrderableLocalDate.fromString("2016-01-01")
    dt should matchPattern { case Success(_) => }
  }
  "01/01/2016" should "result in date" in {
    implicit val pattern = "MM/dd/uuuu"
    val dt = OrderableLocalDate.fromString("01/01/2016")
    dt should matchPattern { case Success(_) => }
  }
  "01/01/2016" should "result in LocalDate using via" in {
    val dt = LocalDate of(2016, 2, 1)
    val m: Map[String, LocalDate] = Map("k" -> dt)
    implicit val pattern = "MM/dd/uuuu"
    val dt2 = OrderableLocalDate.viaLookup("k", m get)
    dt2 should matchPattern { case Success(_) => }
  }
  "zero" should "result in date now" in {
    implicit val pattern = ""
    val dt1 = OrderableLocalDate zero
    val dt2 = LocalDate.now()
    dt1 shouldBe dt2
  }
  "compare LocalDates 2016/02/01 2016/02/01 " should "result zero" in {
    val dt1 = LocalDate of(2016, 2, 1)
    val dt2 = LocalDate of(2016, 2, 1)
    OrderableLocalDate compare(dt1, dt2) shouldBe 0
  }
}
