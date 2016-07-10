package com.phasmid.laScala

import com.phasmid.laScala.Orderable.OrderableDate
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
  * @author scalaprof
  */
class OrderableSpec extends FlatSpec with Matchers {
  "2016-01-01" should "result in date" in {
    implicit val pattern = ""
    val dt = OrderableDate.fromString("2016-01-01")
    dt should matchPattern { case Success(_) => }
  }
  "01/01/2016" should "result in date" in {
    implicit val pattern = "MM/dd/uuuu"
    val dt = OrderableDate.fromString("01/01/2016")
    dt should matchPattern { case Success(_) => }
  }
}
