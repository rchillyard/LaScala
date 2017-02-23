/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.parser

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util._

/**
  * Created by scalaprof on 9/13/16.
  */
class IngestibleSpec extends FlatSpec with Matchers {

  behavior of "ingest"

  it should "work for Int" in {
    trait IngestibleInt extends Ingestible[Int] {
      def fromString(w: String): Try[Int] = Try(w.toInt)
    }
    implicit object IngestibleInt extends IngestibleInt
    val source = Source.fromChars(Array('x', '\n', '4', '2'))
    val ingester = new Ingest[Int]()
    val xys = ingester(Ingest.ignoreHeader(source)).toSeq
    xys.size shouldBe 1
    xys.head should matchPattern { case Success(42) => }
  }

  it should "work for names" in {
    trait IngestibleName extends Ingestible[Name] {
      private val nameR = """(\w+) (\w+)""".r

      def fromString(w: String): Try[Name] = Try(w match {
        case nameR(f, l) => Name(f, l)
        case _ => throw new Exception(s"$w cannot be interpreted as a name")
      })
    }
    implicit object IngestibleName extends IngestibleName
    Try(Source.fromString("Tom Brady\nBill Bellichik")) match {
      case Success(source) =>
        val ingester = new Ingest[Name]()
        val nys: Seq[Try[Name]] = (for (ny <- ingester(source)) yield ny.transform(
          { ny => Success(ny) }, { e => System.err.println(e); ny }
        )).toSeq
        val nos: Seq[Option[Name]] = for (ny <- nys) yield for (n <- ny.toOption) yield n
        val ns = nos.flatten
        ns.size shouldBe 2
        ns.head should matchPattern { case Name("Tom", "Brady") => }
        source.close()
      case Failure(x) =>
        fail(x)
    }
  }

  case class Name(first: String, last: String)

}
