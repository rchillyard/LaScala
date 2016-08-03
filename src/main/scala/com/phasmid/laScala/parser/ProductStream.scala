package com.phasmid.laScala.parser

import java.io.{File, InputStream}
import java.net.URI

import com.phasmid.laScala.{FP, Trial, Tuples}

// For now, until we can convert to Java8 datetimes
import org.joda.time._
import org.joda.time.format._

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce
import scala.io.Source
import scala.util._
import scala.util.parsing.combinator._

/**
  * ProductStream is a monadic trait which provides a Stream of Tuples (tuples) and a Header (header).
  * Thus it is well-suited for use as an ingest mechanism of CSV files.
  *
  * The polymorphic type X is a Tuple of some count.
  * Please note that type inference is not able to infer this type from reading the file (which doesn't happen until runtime).
  * Therefore, the caller must supply the type of X.
  * Please see ProductStreamSpec for exemplars.
  *
  * Please see inline method documentation for details of other methods.
  *
  * @author scalaprof
  * @tparam X the underlying type of this product stream: must be a sub-type of Product, that's to say, a Tuple or something similar
  */
trait ProductStream[X <: Product] {
  /**
    * @return a sequence of String corresponding to the column names (left to right)
    */
  def header: Seq[String]

  /**
    * @return a Stream of tuples
    */
  def tuples: Stream[X]

  /**
    * @return a materialized (non-lazy) List version of the tuples.
    */
  lazy val asList = tuples.toList

  /**
    * map method
    *
    * @param f function to be applied to each tuple
    * @return a ProductStream of transformed tuples
    */
  def map[Y <: Product](f: X => Y): ProductStream[Y] =
    ConcreteProductStream[Y](header, tuples map f)

  /**
    * flatMap method
    *
    * @param f function to be applied to each tuple
    * @return a ProductStream of transformed tuples
    */
  def flatMap[Y <: Product](f: X => GenTraversableOnce[Y]): ProductStream[Y] =
    ConcreteProductStream[Y](header, tuples flatMap f)

  /**
    * toMap method
    *
    * @param pk function to yield a primary key value from a tuple
    * @return a Map where each element is of form pk->tuple
    */
  def toMap[K](pk: X => K): Map[K, X] = (for {x <- asList} yield pk(x) -> x).toMap

  /**
    * @param i get the ith row as a tuple
    * @return Some(tuple) if i is valid, else None
    */
  def get(i: Int): Option[X] = if (i >= 0 && i < asList.size) Some(asList.apply(i)) else None

  /**
    * @return a Stream of Maps, each map corresponding to a row, such that the keys are the column names (from the header)
    *         and the values are the tuple values
    */
  def asMaps: Stream[Map[String, Any]]
}

/**
  * Base class for implementers of ProductStream
  */
abstract class ProductStreamBase[X <: Product] extends ProductStream[X] {
  /**
    * Method asMaps converts this ProductStream into a Stream of Map[String,Any] objects, one per row.
    * The keys for the map are derived from the header and the values from the tuple elements.
    *
    * @return a Stream of Map[String,Any] objects
    */
  def asMaps: Stream[Map[String, Any]] =
    tuples map { t => (t.productIterator zip header.toIterator map { case (v, k) => k -> v }).toMap }
}

/**
  * Base class for ProductStream which additionally derive their header and tuples from parsing a Stream of Strings (one per row).
  */
abstract class TupleStreamBase[X <: Product](parser: CsvParser, input: Stream[String]) extends ProductStreamBase[X] {
  /**
    * @return the header for this object
    * @throws the exception which is wrapped in a Failure from wsy (below)
    */
  def header: Seq[String] = {
//    val seq = wsy.getOrElse(Seq[String]())
    val seq = wsy match {
      case Success(x) => println(s"header: $x");x
      case Failure(t) => println(s"failure: $t"); Seq[String]()
    }
    println(s"seq: $seq")
    seq}

  /**
    * @param f the function which will be applied to a String to yield an Any (an element of a Tuple)
    * @param s the (row/line) String to be parsed
    * @return a Option[X]
     */
  def stringToTuple(f: String => Try[Any])(s: String): Option[X] = {
    val triedX1 = stringToTryTuple(f)(s)
    println(s"triedX1: $triedX1")
    triedX1.toOption
  }

  protected lazy val wsy: Try[Seq[String]] = parser.parseRow(headerRow)

  protected def headerRow: String

  private def stringToTryTuple(f: String => Try[Any])(s: String): Try[X] = {
    val row = parser.parseRow(s)
    println(s"row: $row, header=$header")
    def rowIsCompatible(ws: List[String]): Boolean = {
      println(s"rowIsCompatible: ws=$ws, header=$header")
      println(s"header.size=${header.size}")
      header.isEmpty || ws.size == header.size
    }
    for {
      ws <- row
       // Note that the following will result in a Failure[NoSuchElementException] if the filter yields false
      if rowIsCompatible(ws)
      // Note that the specification of [X] in the following is essential
      x <- TupleStream.seqToTuple[X](ws)(f)
    } yield x
  }
}

/**
  * Case class which implements ProductStream where the header and tuples are specified directly
  */
case class ConcreteProductStream[X <: Product](header: Seq[String], tuples: Stream[X]) extends ProductStreamBase[X]

/**
  * Case class which implements ProductStream where the header and tuples are specified indirectly, by providing
  * a parser and Stream[String] such that the element types of the resulting tuples will be inferred from their representative
  * Strings.
  *
  * @tparam X a Tuple which should correspond with the number of (and types inferred from) the values.
  */
case class CSV[X <: Product](parser: CsvParser, input: Stream[String], hasHeader: Boolean) extends TupleStreamBase[X](parser, input) {

  protected def headerRow: String = if (hasHeader) input.head else ""

  /**
    * Method to define the tuples of this TupleStreamBase object.
    *
    * @return a Stream of [X] objects
    */
  def tuples: Stream[X] = {
    val stream = if (hasHeader) input.tail else input
    println(s"stream: $stream")
    (stream map (stringToTuple(parser.elementParser))).flatten
  }

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param key the name of the column
    * @return an Option of Stream[Y] where Y is the type of the column
    */
  def column[Y](key: String): Option[Stream[Y]] = column(header.indexOf(key))

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param i the index of the column (0 on the left, n-1 on the right)
    * @return an Option of Stream[Y] where Y is the type of the column
    */
  def column[Y](i: Int): Option[Stream[Y]] =
    if (i >= 0) Some(tuples map CSV.project[X, Y](i))
    else None
}

/**
  * Case class which implements ProductStream where the header and tuples are specified indirectly, by providing
  * a parser and Stream[String] such that the element types of the resulting tuples will be Strings.
  *
  * @tparam X a Tuple which should correspond with the number of values (all types of the tuple should be String).
  */
case class TupleStream[X <: Product](parser: CsvParser, input: Stream[String], hasHeader: Boolean) extends TupleStreamBase[X](parser, input) {
  def tuples = ((if (hasHeader) input.tail else input) map stringToTuple { x => Success(x) }).flatten

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param key the name of the column
    * @return an Option of Stream[String]
    */
  def column(key: String): Option[Stream[String]] = column(header.indexOf(key))

  /**
    * method to project ("slice") a ProductStream into a single column
    *
    * @param i the index of the column (0 on the left, n-1 on the right)
    * @return an Option of Stream[String]
    */
  def column(i: Int): Option[Stream[String]] =
    if (i >= 0) Some(tuples map TupleStream.project[X](i))
    else None

  protected def headerRow: String = if (hasHeader) input.head else ""
}

object TupleStream {
  def apply[X <: Product](input: Stream[String], hasHeader: Boolean): TupleStream[X] = apply(CsvParser(), input, hasHeader)

  def apply[X <: Product](parser: CsvParser, input: InputStream, hasHeader: Boolean): TupleStream[X] = apply(parser, Source.fromInputStream(input).getLines.toStream, hasHeader)

  def apply[X <: Product](input: InputStream, hasHeader: Boolean): TupleStream[X] = apply(CsvParser(), input, hasHeader)

  def apply[X <: Product](parser: CsvParser, input: File, hasHeader: Boolean): TupleStream[X] = apply(parser, Source.fromFile(input).getLines.toStream, hasHeader)

  def apply[X <: Product](input: File, hasHeader: Boolean): TupleStream[X] = apply(CsvParser(), input, hasHeader)

  def apply[X <: Product](parser: CsvParser, input: URI, hasHeader: Boolean): TupleStream[X] = apply(parser, Source.fromFile(input).getLines.toStream, hasHeader)

  def apply[X <: Product](input: URI, hasHeader: Boolean): TupleStream[X] = apply(CsvParser(), input, hasHeader)

  def project[X <: Product](i: Int)(x: X): String = x.productElement(i).asInstanceOf[String]

  def toTuple[X <: Product](ats: Seq[Try[Any]]): Try[X] =
    for (as <- FP.sequence(ats)) yield Tuples.toTuple(as).asInstanceOf[X]

  def seqToTuple[X <: Product](ws: Seq[String])(f: String => Try[Any]): Try[X] = {
    val r = toTuple(ws map f)
    println(s"seqToTuple: ws: $ws; r: $r")
    r
  }
}

object CSV {
  def apply[X <: Product](input: Stream[String], hasHeader: Boolean): CSV[X] = apply(CsvParser(), input, hasHeader)

  def apply[X <: Product](parser: CsvParser, input: InputStream, hasHeader: Boolean): CSV[X] = apply(parser, Source.fromInputStream(input).getLines.toStream, hasHeader)

  def apply[X <: Product](input: InputStream, hasHeader: Boolean): CSV[X] = apply(CsvParser(), input, hasHeader)

  def apply[X <: Product](parser: CsvParser, input: File, hasHeader: Boolean): CSV[X] = apply(parser, Source.fromFile(input).getLines.toStream, hasHeader)

  def apply[X <: Product](input: File, hasHeader: Boolean): CSV[X] = apply(CsvParser(), input, hasHeader)

  def apply[X <: Product](parser: CsvParser, input: URI, hasHeader: Boolean): CSV[X] = apply(parser, Source.fromFile(input).getLines.toStream, hasHeader)

  def apply[X <: Product](input: URI, hasHeader: Boolean): CSV[X] = apply(CsvParser(), input, hasHeader)

  def project[X <: Product, Y](i: Int)(x: X) = x.productElement(i).asInstanceOf[Y]
}

abstract class CsvParserBase(f: String => Try[Any]) extends JavaTokenParsers {
  /**
    * @return the trial function that will convert a String into Try[Any]
    *         This method is referenced only by CSV class (not by TupleStream, which does no element conversion).
    */
  def elementParser = f
}

case class CsvParser(
                      delimiter: String = ",", // delimiter separating elements within rows
                      quoteChar: String = """"""", // quotation char to allow strings to include literal delimiter characters, decimal points, etc.
                      parseElem: String => Try[Any] = CsvParser.defaultParser
                    ) extends CsvParserBase(parseElem) {
  def row: Parser[List[String]] = repsep(term, delimiter)

  def term: Parser[String] = quoteChar ~> s"[^$quoteChar]*".r <~ quoteChar | s"[^$delimiter]*".r

  def parseRow(s: String): Try[List[String]] = if (s.isEmpty) scala.util.Success(List[String]())
  else this.parseAll(this.row, s) match {
    case this.Success(r, _) => scala.util.Success(r)
    case f@(this.Failure(_, _) | this.Error(_, _)) => scala.util.Failure(new Exception(s"cannot parse $s: $f"))
  }
}

object CsvParser {
  val dateFormatStrings = Seq("yyyy-MM-dd", "yyyy-MM-dd-hh:mm:ss.s")
  // etc.
  val dateParser = Trial[String, Any]((parseDate _) (dateFormatStrings))
  val defaultParser = Trial.none[String, Any] :| { case s@(date0(_) | date4(_) | date1(_)) => dateParser(s) } :^ { case quoted(w) => w } :^ { case whole(s) => s.toInt } :^ { case truth(w) => true } :^ { case untruth(w) => false } :^ { case s => s match {
    case floating(_) | floating(_, _) | floating(_, _, _) => s.toDouble
  }
  } :^ { case s => s }
  val date0 = """(\d\d\d\d-\d\d-\d\d)""".r
  // ISO 8601
  val date1 =
    """(?m)^(0[1-9]|1\d|2[0-8]|29(?=-\d\d-(?!1[01345789]00|2[1235679]00)\d\d(?:[02468][048]|[13579][26]))|30(?!-02)|31(?=-0[13578]|-1[02]))-(0[1-9]|1[0-2])-([12]\d{3}) ([01]\d|2[0-3]):([0-5]\d):([0-5]\d)$""".r
  val date2 = """(?m)^\d{4}-(((0[13578]|1[02])-(0[1-9]|[12]\d|3[0-1]))|(02-(0[1-9]|[12]\d))|((0[469]|11)-(0[1-9]|[12]\d|30)))$""".r
  val date3 = """(^(((\d\d)(([02468][048])|([13579][26]))-02-29)|(((\d\d)(\d\d)))-((((0\d)|(1[0-2]))-((0\d)|(1\d)|(2[0-8])))|((((0[13578])|(1[02]))-31)|(((0[1,3-9])|(1[0-2]))-(29|30)))))\s(([01]\d|2[0-3]):([0-5]\d):([0-5]\d))$)""".r
  val date4 = """(?mi)^([\+-]?\d{4}(?!\d{2}\b))((-?)((0[1-9]|1[0-2])(\3([12]\d|0[1-9]|3[01]))?|W([0-4]\d|5[0-2])(-?[1-7])?|(00[1-9]|0[1-9]\d|[12]\d{2}|3([0-5]\d|6[1-6])))([T\s]((([01]\d|2[0-3])((:?)[0-5]\d)?|24\:?00)([\.,]\d+(?!:))?)?(\17[0-5]\d([\.,]\d+)?)?([zZ]|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?)?)?$""".r

  def parseDate(dfs: Seq[String])(s: String): Try[DateTime] = {
    @tailrec def loop(formats: Seq[DateTimeFormatter], result: Try[DateTime]): Try[DateTime] = formats match {
      case Nil => result
      case h :: t => loop(t, result orElse Try(h.parseDateTime(s)))
    }
    loop(dfs map {
      DateTimeFormat.forPattern
    }, Failure(new Exception(s""""$s" cannot be parsed as date""")))
  }

  val quoted = """"([^"]*)"""".r
  val whole = """(\d+)""".r
  val floating = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  val truth = """(?i)^([ty]|true|yes)$""".r
  val untruth = """(?i)^([fn]|false|no)$""".r
}

