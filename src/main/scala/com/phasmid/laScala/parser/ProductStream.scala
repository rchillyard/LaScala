package com.phasmid.laScala.parser

import java.io.{File, InputStream}
import java.net.{URI, URL}

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

  def carper(s: String): Unit

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
    * filter method
    *
    * @param p predicate to be applied to each tuple
    * @return a product stream which contains all of the elements from this that satisfy the predicate
    */
  def filter(p: X => Boolean): ProductStream[X] = ConcreteProductStream[X](header, tuples filter p)

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

object ProductStream {
}

/**
  * Base class for implementers of ProductStream
  */
abstract class ProductStreamBase[X <: Product] extends ProductStream[X] {
  /**
    * Default carper. Normally, programmers should override this with a call to a logger
    *
    * @param s the String to carp about
    */
  override def carper(s: String): Unit = System.err.println(s)

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
  * Abstract class for ProductStream which additionally derives their header and tuples from parsing a Stream of Strings (one per row).
  */
abstract class TupleStreamBase[X <: Product](parser: CsvParser, input: Stream[String]) extends ProductStreamBase[X] {
  /**
    * @param f the function which will be applied to a String to yield an Any (an element of a Tuple)
    * @param s the (row/line) String to be parsed
    * @return a Option[X]
    */
  def stringToTuple(f: String => Try[Any])(s: String): Try[X] = stringToTryTuple(f)(s)

  protected lazy val wsy: Try[Seq[String]] = parser.parseRow(headerRow)

  protected def headerRow: String

  // CONSIDER inlining this method
  private def stringToTryTuple(f: String => Try[Any])(s: String): Try[X] = {
    def rowIsCompatible(ws: List[String]): Boolean = header.isEmpty || ws.size == header.size
    for {
      ws <- parser.parseRow(s)
      // Note that the following will result in a Failure[NoSuchElementException] if the filter yields false
      if rowIsCompatible(ws)
      // Note that the specification of [X] in the following is essential
      x <- TupleStream.seqToTuple[X](ws)(f)
    } yield x
  }
}

/**
  * Case class which implements ProductStream where the header and tuples are specified directly
  *
  * @param header the header (names of columns)
  * @param tuples the tuples, as a Stream
  * @tparam X the tuple type
  */
case class ConcreteProductStream[X <: Product](header: Seq[String], tuples: Stream[X]) extends ProductStreamBase[X]

/**
  * Case class which implements ProductStream where the header and tuples are specified indirectly, by providing
  * a parser and Stream[String] such that the element types of the resulting tuples will be inferred from their representative
  * Strings.
  *
  * @tparam X a Tuple which should correspond with the number of (and types inferred from) the values.
  */
case class CSV[X <: Product](parser: CsvParser, input: Stream[String], optionalHeader: Option[Seq[String]]) extends TupleStreamBase[X](parser, input) {

  /**
    * May print to the SysErr stream as a side-effect if wsy is a Failure
    *
    * @return the header for this object
    */
  def header: Seq[String] = optionalHeader.getOrElse(wsy match {
    case Success(x) => x
    case Failure(t) => carper(s"failure: $t"); Seq[String]()
  })

  protected def headerRow: String = if (optionalHeader.isEmpty) input.head else ""

  /**
    * Method to define the tuples of this TupleStreamBase object.
    *
    * @return a Stream of [X] objects
    */
  def tuples: Stream[X] = {
    val xts = getStream map stringToTuple(parser.elementParser)
    for (xt <- xts; x <- xt.recoverWith({ case t => carper(t.getLocalizedMessage); Failure(new Exception("logged already")) }).toOption) yield x
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

  def getStream: Stream[String] = if (optionalHeader.isEmpty) input.tail else input
}

/**
  * Case class which implements ProductStream where the header and tuples are specified indirectly, by providing
  * a parser and Stream[String] such that the element types of the resulting tuples will be Strings.
  *
  * @tparam X a Tuple which should correspond with the number of values (all types of the tuple should be String).
  */
case class TupleStream[X <: Product](parser: CsvParser, input: Stream[String], optionalHeader: Option[Seq[String]]) extends TupleStreamBase[X](parser, input) {

  /**
    * May print to the SysErr stream as a side-effect if wsy is a Failure
    *
    * @return the header for this object
    */
  def header: Seq[String] = optionalHeader.getOrElse(wsy match {
    case Success(x) => x
    case Failure(t) => carper(s"failure: $t"); Seq[String]()
  })

  def tuples: Stream[X] = for (t <- getStream map stringToTuple { x => Success(x) }; x <- t.toOption) yield x

  def getStream: Stream[String] = if (optionalHeader.isEmpty) input.tail else input

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

  protected def headerRow: String = if (optionalHeader.isEmpty) input.head else ""
}

object TupleStream {
  def apply[X <: Product](input: Stream[String]): TupleStream[X] = apply(input, None)

  def apply[X <: Product](input: Stream[String], optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(CsvParser(), input, optionalHeader)

  //  def apply[X <: Product](parser: CsvParser, input: Stream[String], optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(parser, input, optionalHeader)

  def apply[X <: Product](input: InputStream): TupleStream[X] = apply(input, None)

  def apply[X <: Product](input: InputStream, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: InputStream, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(parser, Source.fromInputStream(input).getLines.toStream, optionalHeader)

  def apply[X <: Product](input: File): TupleStream[X] = apply(input, None)

  def apply[X <: Product](input: File, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: File, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(parser, Source.fromFile(input).getLines.toStream, optionalHeader)

  def apply[X <: Product](input: URL): TupleStream[X] = apply(input, None)

  def apply[X <: Product](input: URL, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: URL, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(parser, Source.fromInputStream(input.openStream).getLines.toStream, optionalHeader)

  def apply[X <: Product](input: URI): TupleStream[X] = apply(input, None)

  def apply[X <: Product](input: URI, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: URI, optionalHeader: Option[Seq[String]]): TupleStream[X] = apply(parser, Source.fromFile(input).getLines.toStream, optionalHeader)

  def project[X <: Product](i: Int)(x: X): String = x.productElement(i).asInstanceOf[String]

  // CONSIDER improving this but be careful because it's easy to get a Try satisfying an Any
  def toTuple[X <: Product](ats: Seq[Try[Any]]): Try[X] = {
    val ast = FP.sequence(ats)
    ast match {
      case Success(as) => Success(Tuples.toTuple(as).asInstanceOf[X])
      case Failure(t) => ast.asInstanceOf[Try[X]]
    }
  }
  def seqToTuple[X <: Product](ws: Seq[String])(f: String => Try[Any]): Try[X] = toTuple(ws map f)
}

object CSV {
  def apply[X <: Product](input: Stream[String]): CSV[X] = apply(input, None)

  def apply[X <: Product](input: Stream[String], optionalHeader: Option[Seq[String]]): CSV[X] = apply(CsvParser(), input, optionalHeader)

  //  def apply[X <: Product](parser: CsvParser, input: Stream[String], optionalHeader: Option[Seq[String]]): CSV[X] = apply(parser, input, optionalHeader)

  def apply[X <: Product](input: InputStream): CSV[X] = apply(input, None)

  def apply[X <: Product](input: InputStream, optionalHeader: Option[Seq[String]]): CSV[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: InputStream, optionalHeader: Option[Seq[String]]): CSV[X] = apply(parser, Source.fromInputStream(input).getLines.toStream, optionalHeader)

  def apply[X <: Product](input: File): CSV[X] = apply(input, None)

  def apply[X <: Product](input: File, optionalHeader: Option[Seq[String]]): CSV[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: File, optionalHeader: Option[Seq[String]]): CSV[X] = apply(parser, Source.fromFile(input).getLines.toStream, optionalHeader)

  def apply[X <: Product](input: URL): CSV[X] = apply(input, None)

  def apply[X <: Product](input: URL, optionalHeader: Option[Seq[String]]): CSV[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: URL, optionalHeader: Option[Seq[String]]): CSV[X] = apply(parser, Source.fromInputStream(input.openStream).getLines.toStream, optionalHeader)

  def apply[X <: Product](input: URI): CSV[X] = apply(input, None)

  def apply[X <: Product](input: URI, optionalHeader: Option[Seq[String]]): CSV[X] = apply(CsvParser(), input, optionalHeader)

  def apply[X <: Product](parser: CsvParser, input: URI, optionalHeader: Option[Seq[String]]): CSV[X] = apply(parser, Source.fromFile(input).getLines.toStream, optionalHeader)

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

  /**
    * Term, which is a String, quoted or not. We wish to preserve the quotation marks if present
    *
    * @return
    */
  def term: Parser[String] = (quoteChar ~ s"[^$quoteChar]*".r ~ quoteChar | s"[^$delimiter]*".r) ^^ { case q1 ~ s ~ q2 => q1.toString + s + q2; case s => s.toString }

  def parseRow(s: String): Try[List[String]] = if (s.isEmpty) scala.util.Success(List[String]())
  else parseAll(row, s) match {
    case Success(r, _) => scala.util.Success(r)
    case f@(Failure(_, _) | Error(_, _)) => scala.util.Failure(new Exception(s"cannot parse $s: $f"))
  }
}

object CsvParser {
  val dateFormatStrings = Seq("y-M-d", "M/d/y", "y-M-d-h:m:s.s")
  // etc.
  val dateParser = Trial[String, Any]((parseDate _) (dateFormatStrings))

  /**
    * The default parser will parse most dates, will parse quoted strings, integers, booleans and floating point values (as Double).
    * If it cannot match on of those patterns, it will fail.
    * NOTE: it is important that the final operator is :|
    */
  val defaultParser: Trial[String, Any] = Trial.none[String, Any] :|
    // here we allow for the possibility of date being enclosed in quotes -- need to generalize
    { case date2(s) => dateParser(s) } :|
    { case s@(date0(_) | date1(_) | date6(_) | dateISO(_)) => dateParser(s) } :^
    { case quoted(w) => w } :^
    { case whole(s) => s.toInt } :^
    { case truth(w) => true } :^
    { case untruth(w) => false } :^
    { case s => s match {
        case floating(_) | floating(_, _) | floating(_, _, _) => s.toDouble
      }
    } :|
    Trial.none[String, Any]
  /**
    * The lax parser is essentially the same as the defaultParser.
    * However, if it cannot match on of those patterns, it will succeed, returning the string as is.
    */
  val laxParser = defaultParser :^ { case s => s }
  val date0 = """^(\d{2,4}-\d{1,2}-\d{1,2})$""".r
  val date1 = """^(\d{1,2}\/\d{1,2}\/\d{2,4})$""".r
  // CONSIDER: date2 is basically the same as date1 but enclosed in quotes. We should allow that for all date formats
  val date2 = """^\"(\d{1,2}\/\d{1,2}\/\d{2,4})\"$""".r
  // ISO 8601
  val dateISO =
    """(?m)^(0[1-9]|1\d|2[0-8]|29(?=-\d\d-(?!1[01345789]00|2[1235679]00)\d\d(?:[02468][048]|[13579][26]))|30(?!-02)|31(?=-0[13578]|-1[02]))-(0[1-9]|1[0-2])-([12]\d{3}) ([01]\d|2[0-3]):([0-5]\d):([0-5]\d)$""".r
  val date4 = """(?m)^\d{4}-(((0[13578]|1[02])-(0[1-9]|[12]\d|3[0-1]))|(02-(0[1-9]|[12]\d))|((0[469]|11)-(0[1-9]|[12]\d|30)))$""".r
  val date5 = """(^(((\d\d)(([02468][048])|([13579][26]))-02-29)|(((\d\d)(\d\d)))-((((0\d)|(1[0-2]))-((0\d)|(1\d)|(2[0-8])))|((((0[13578])|(1[02]))-31)|(((0[1,3-9])|(1[0-2]))-(29|30)))))\s(([01]\d|2[0-3]):([0-5]\d):([0-5]\d))$)""".r
  val date6 = """(?mi)^([\+-]?\d{4}(?!\d{2}\b))((-?)((0[1-9]|1[0-2])(\3([12]\d|0[1-9]|3[01]))?|W([0-4]\d|5[0-2])(-?[1-7])?|(00[1-9]|0[1-9]\d|[12]\d{2}|3([0-5]\d|6[1-6])))([T\s]((([01]\d|2[0-3])((:?)[0-5]\d)?|24\:?00)([\.,]\d+(?!:))?)?(\17[0-5]\d([\.,]\d+)?)?([zZ]|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?)?)?$""".r

  def parseDate(dfs: Seq[String])(s: String): Try[DateTime] = {
    @tailrec def loop(formats: Seq[DateTimeFormatter], result: Try[DateTime]): Try[DateTime] = formats match {
      case Nil => result
      case h :: t => loop(t, result orElse Try(h.parseDateTime(s)))
    }
    loop(dfs map {
      DateTimeFormat.forPattern
    }, Failure(new Exception(s""""$s" cannot be parsed as date""")))
  }

  val quoted = """\"([^"]*)\"""".r
  val whole = """(\d+)""".r
  val floating = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  val truth = """(?i)^([ty]|true|yes)$""".r
  val untruth = """(?i)^([fn]|false|no)$""".r
}

