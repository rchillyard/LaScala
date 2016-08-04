package com.phasmid.laScala.parser

import java.io.{File, FileInputStream}
import java.net.URL

import com.phasmid.laScala.Lift
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.collection.Map
import scala.util._

/**
  * @author scalaprof
  */
class ProductStreamSpec extends FlatSpec with Matchers {
  """"Hello", "World!"""" should "be (String) stream via CSV" in {
    val stream = Stream(Tuple1("Hello"), Tuple1("World!"))
    val c = ConcreteProductStream[Tuple1[String]](Seq("word"),stream)
    c.header shouldBe List("word")
    val wts = c.tuples
    wts.head match {
      case Tuple1(s) => assert(s == "Hello")
    }
    wts.tail.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
}


