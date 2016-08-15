package com.phasmid.laScala.parser

import java.time.LocalDate

import com.phasmid.laScala.Lift
import com.phasmid.laScala.values._
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.{Success, Try}

/**
  * Created by scalaprof on 8/3/16.
  */
class CsvParserSpec extends FlatSpec with Matchers with Inside {
  val defaultParser = CsvParser()

  "term" should """parse "x",y """ in {
    val s = """"x",y"""
    defaultParser.parse(defaultParser.term,s) should matchPattern { case defaultParser.Success(""""x"""",_) => }
    // we ignore the ,y part
  }
  "row" should """parse "x",y """ in {
    val s = """"x",y"""
    defaultParser.parseAll(defaultParser.row,s) should matchPattern { case defaultParser.Success(List(""""x"""","y"),_) => }
  }
  "CsvParser()" should """parse "x" as Success(List("x"))""" in {
    defaultParser.parseRow(""""x"""") should matchPattern { case scala.util.Success(List(""""x"""")) => }
  }
  it should """parse "x,y" as Success(List("x,y"))""" in {
    defaultParser.parseRow(""""x,y"""") should matchPattern { case scala.util.Success(List(""""x,y"""")) => }
  }
  it should """parse "x,y" as Success(List("x","y")""" in {
    defaultParser.parseRow("x,y") should matchPattern { case scala.util.Success(List("x", "y")) => }
  }
  val pipeParser = CsvParser("|")
  """"CsvParser("|")"""" should """parse "|" as Success(List("|"))""" in {
    pipeParser.parseRow(""""|"""") should matchPattern { case scala.util.Success(List(""""|"""")) => }
  }
  it should """parse x,y as Success(List("x,y"))""" in {
    pipeParser.parseRow("x,y") should matchPattern { case scala.util.Success(List("x,y")) => }
  }
  it should """parse x,y as Success(List("x","y")""" in {
    pipeParser.parseRow("x|y") should matchPattern { case scala.util.Success(List("x", "y")) => }
  }
  val customParser = CsvParser("|", "'")
  """"CsvParser("|","'")"""" should """parse '|' as Success(List("|"))""" in {
    customParser.parseRow("'|'") should matchPattern { case scala.util.Success(List("'|'")) => }
  }
  it should """parse x,y as Success(List("x,y"))""" in {
    customParser.parseRow("x,y") should matchPattern { case scala.util.Success(List("x,y")) => }
  }
  it should """parse x,y as Success(List("x","y")""" in {
    customParser.parseRow("x|y") should matchPattern { case scala.util.Success(List("x", "y")) => }
  }

  "CsvParser.parseElem" should "parse 1 as 1" in (CsvParser.defaultParser("1") should matchPattern { case Success(IntScalar(1, _)) => })
  it should "parse 1.0 as 1.0" in (CsvParser.defaultParser("1.0") should matchPattern { case Success(DoubleScalar(1.0, _)) => })
  it should "parse true as true" in (CsvParser.defaultParser("true") should matchPattern { case Success(BooleanScalar(true, _)) => })
  it should "parse false as false" in (CsvParser.defaultParser("false") should matchPattern { case Success(BooleanScalar(false, _)) => })
  it should "parse yes as yes" in (CsvParser.defaultParser("yes") should matchPattern { case Success(BooleanScalar(true, _)) => })
  it should "parse no as false" in (CsvParser.defaultParser("no") should matchPattern { case Success(BooleanScalar(false, _)) => })
  it should "parse T as true" in (CsvParser.defaultParser("T") should matchPattern { case Success(BooleanScalar(true, _)) => })
  // TODO why does this give StringScalar, not QuotedStringScalar
  it should """parse "1" as "1"""" in (CsvParser.defaultParser(""""1"""") should matchPattern { case Success(StringScalar("1", _)) => })
  it should """parse 2016-3-8 as datetime""" in {
    val dt = CsvParser.defaultParser("2016-3-8")
    dt should matchPattern { case Success(d: DateScalar) => }
    dt.get shouldBe DateScalar(2016, 3, 8)
  }

  def putInQuotes(w: String): Scalar = s"""'$w'"""

  val customElemParser = CsvParser(parseElem = Lift(putInQuotes _))
  "custom element parser" should "parse 1 as '1'" in (customElemParser.elementParser("1") should matchPattern { case Success(StringScalar("'1'", _)) => })
  it should "parse 1.0 as '1.0'" in (customElemParser.elementParser("1.0") should matchPattern { case Success(StringScalar("'1.0'", _)) => })
  it should "parse true as 'true'" in (customElemParser.elementParser("true") should matchPattern { case Success(StringScalar("'true'", _)) => })
  it should """parse "1" as '"1"'""" in (customElemParser.elementParser(""""1"""") should matchPattern { case Success(StringScalar("""'"1"'""", _)) => })

  "CsvParser.parseDate" should "work" in {
    val dt = CsvParser.parseDate(CsvParser.dateFormatStrings)("2016-03-08")
    dt should matchPattern { case Success(x) => }
    dt.get shouldBe DateScalar(2016, 3, 8)
  }

  "quoted string" should "work" in {
    val appleInc: Try[Any] = CsvParser.defaultParser(""""Apple Inc."""")
    appleInc should matchPattern { case Success(StringScalar("Apple Inc.", _)) => }
  }
  "content of quotes.csv" should "work" in {
    def parser(s: String): Try[Scalar] = CsvParser.defaultParser(s)
    val row = """"Apple Inc.",104.48,"8/2/2016",12.18"""
    val xy: Try[(String, Double, LocalDate, Double)] = for {
      ws <- defaultParser.parseRow(row)
      x <- TupleStream.seqToTuple[(String, Double, LocalDate, Double)](ws)(parser _)
    } yield x
    xy should matchPattern { case Success(("Apple Inc.",104.48,_,12.18)) => }
  }
  """dateParser""" should "work" in {
    val dp = CsvParser.dateParser
    dp("2016-03-15") should matchPattern { case Success(_) => }
  }

  it should "work for format d-MMM-yy" in {
    val dp = CsvParser.dateParser
    dp("18-Oct-15") should matchPattern { case Success(_) => }
  }

}
