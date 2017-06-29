package com.phasmid.laScala.parser

import java.io.{File, FileInputStream}
import java.net.URL
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.values.{IntScalar, Scalar, StringScalar}
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.collection.Map
import scala.util.Success

/**
  * @author scalaprof
  */
class CSVSpec extends FlatSpec with Matchers with Inside {
  val defaultParser = CsvParser()
  """"Hello", "World!"""" should "be (String) stream via CSV" in {
    val c = CSV[Tuple1[String]](Stream("x",""""Hello"""", """"World!""""))
    c.header shouldBe Header(List("x"))
    val wts = c.tuples
    wts.head match {
      case Tuple1(s) => assert(s == "Hello")
    }
    wts.tail.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
  it should "convert to list properly" in {
    val c = CSV[Tuple1[String]](Stream("x",""""Hello"""", """"World!""""))
    val wts = c.asList
    wts.size should be(2)
  }
  it should "convert to map properly" in {
    val c = CSV[Tuple1[String]](Stream("x",""""Hello"""", """"World!""""))
    val wtIm = c toMap { case Tuple1(s) => s.hashCode }
    wtIm.get("Hello".hashCode) should matchPattern { case Some(Tuple1("Hello")) => }
  }
  it should "have column x of type String" in {
    //noinspection ScalaUnusedSymbol
    val c = CSV[Tuple1[String]](Stream("x",""""Hello"""", """"World!""""))
    // NOTE: this works only with 2.11
    //    c column "x" match {
    //      case Some(xs) =>
    //        xs.take(2).toList.size should be(2)
    //        xs.head shouldBe "Hello"
    //        xs(1) shouldBe "World!"
    //      case _ => fail("no column projected")
    //    }
  }
  """"3,5", "8,13"""" should "be (Int,Int) stream" in {
    val iIts = CSV[(Int, Int)](Stream("x,y", "3,5", "8,13")).tuples
    iIts.head match {
      case (x, y) => assert(x == 3 && y == 5)
    }
    iIts.tail.head match {
      case (x, y) => assert(x == 8 && y == 13)
    }
  }
  it should "have column y of type Int" in {
    // NOTE: this only works with Scala 2.11
    //    CSV[(Int, Int)](Stream("x,y", "3,5", "8,13")) column "y" match {
    //      case Some(ys) =>
    //        ys.take(2).toList.size should be(2)
    //        ys.head shouldBe 5
    //        ys(1) shouldBe 13
    //      case _ => fail("no column projected")
    //    }
  }
  it should "convert to map properly" in {
    val c = CSV[(Int, Int)](Stream("x,y", "3,5", "8,13"))
    val iItIm = c toMap { case (x, _) => x }
    iItIm.get(8) should matchPattern { case Some((8, 13)) => }
  }
  it should "map into (Double,Double) properly" in {
    val c = CSV[(Int, Int)](Stream("x,y", "3,5", "8,13"))
    val doubles = c map { case (x, y) => (x.toDouble, y.toDouble) }
    val dDts = doubles.tuples
    dDts.head match {
      case (x, y) => assert(x == 3.0 && y == 5.0)
    }
  }
  it should "convert into maps properly (1)" in {
    val zWmsy = CSV[(Int, Int)](Stream("x,y", "3,5", "8,13")).asMaps
    zWmsy should matchPattern { case Success(_) => }
    inside(zWmsy) {
      case Success(zWms) =>
        zWms.head should be(Map("x" -> IntScalar(3), "y" -> IntScalar(5)))
        zWms(1) should be(Map("x" -> IntScalar(8), "y" -> IntScalar(13)))
    }
  }
  it should "convert into maps properly (2)" in {
    val zWmsy = CSV[(Int, Int)](Stream("y,x", "3,5", "8,13")).asMaps
    zWmsy should matchPattern { case Success(_) => }
    inside(zWmsy) {
      case Success(zWms) =>
        zWms.head should be(Map("y" -> IntScalar(3), "x" -> IntScalar(5)))
        zWms(1) should be(Map("y" -> IntScalar(8), "x" -> IntScalar(13)))
    }
  }
  """"3,5.0", "8,13.5"""" should "be (Int,Double) stream" in {
    val dIts = CSV[(Int, Double)](Stream("x,y", "3,5.0", "8,13.5")).tuples
    dIts.head match {
      case (x, y) => assert(x == 3 && y == 5.0)
    }
    dIts.tail.head match {
      case (x, y) => assert(x == 8 && y == 13.5)
    }
  }
  """"milestone 1, 2016-3-8", "milestone 2, 2016-3-15"""" should "be (String,Datetime) stream" in {
    val dIts = CSV[(String, LocalDate)](Stream("event,date", """"milestone 1",2016-3-8""", """"milestone 2",2016-3-15""")).tuples
    dIts.head match {
      case (x, y) => assert(x == "milestone 1" && y == LocalDate.parse("2016-03-08"))
    }
    dIts.tail.head match {
      case (x, y) => assert(x == "milestone 2" && y == LocalDate.parse("2016-03-15"))
    }
  }
  "sample.csv" should "be (String,Int) stream using URI" in {
    val sample = getClass.getResource("sample.csv")
    sample should not be null
    val iWts = CSV[(String, Int)](sample.toURI).tuples
    iWts.head match {
      case (q, y) => assert(q == "Sunday" && y == 1)
    }
    iWts.tail.head match {
      case (q, y) => assert(q == "Monday" && y == 2)
    }
    iWts.size should be(8)
    (iWts take 8).toList(7) shouldBe("TGIF, Bruh", 8)
  }
  ignore should "be (String,Int) stream" in {
    val iWts = CSV[(String, Int)](new FileInputStream(new File("src/test/scala/com/phasmid/laScala/parser/sample.csv"))).tuples
    iWts.head match {
      case (x, y) => assert(x == "Sunday" && y == 1)
    }
    iWts.tail.head match {
      case (x, y) => assert(x == "Monday" && y == 2)
    }
    iWts.size should be(8)
    (iWts take 8).toList(7) shouldBe("TGIF, Bruh", 8)
  }
  ignore should "be (String,Int) stream using File" in {
    val iWts = CSV[(String, Int)](new File("sample.csv")).tuples
    iWts.head match {
      case (x, y) => assert(x == "Sunday" && y == 1)
    }
    iWts.tail.head match {
      case (x, y) => assert(x == "Monday" && y == 2)
    }
    iWts.size should be(8)
    (iWts take 8).toList(7) shouldBe("TGIF, Bruh", 8)
  }
  "quotes.csv" should "work from local URL" in {
    val url = getClass.getResource("quotes.csv")
    url should not be null
    val csv = CSV[(String, Double, LocalDate, Double)](defaultParser, url.toURI, Header(List("name", "lastTradePrice", "lastTradeDate", "P/E ratio")))
    val x = csv.tuples
    x.size shouldBe 1
    x.head should matchPattern { case ("Apple Inc.", 104.48, _, 12.18) => }
  }
  // NOTE: you may have to ignore this if you are behind a firewall
  ignore should "work from URL stream" in {
    val url = new URL("http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=nl1d1r&e=.csv")
    val csv = CSV[(String, Double, LocalDate, Double)](url, Header(List("name", "lastTradePrice", "lastTradeDate", "P/E ratio")))
    val x = csv.tuples
    x.size shouldBe 1
    x.head should matchPattern { case ("Apple Inc.", _, _, _) => }
    inside(x.head) {
      case (_, _, ltd, per) =>
        assert(per > 8 && per < 20)
        val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        assert(ltd.isAfter(LocalDate.parse("2016-08-02", formatter)))
    }
  }
  it should "yield maps from local URL" in {
    val url = getClass.getResource("quotes.csv")
    url should not be null
    val csv = CSV[(String, Double, LocalDate, Double)](defaultParser, url.toURI, Header(List("name", "lastTradePrice", "lastTradeDate", "P/E ratio")))
    val xWmsy = csv.asMaps
    inside(xWmsy) {
      case Success(xWms) =>
        xWms.size shouldBe 1
        inside(xWms.head) {
          case m: Map[String, Scalar] =>
            m.size shouldBe 4
            m("name") should matchPattern { case StringScalar("Apple Inc.", _) => }
        }
    }
  }

}


