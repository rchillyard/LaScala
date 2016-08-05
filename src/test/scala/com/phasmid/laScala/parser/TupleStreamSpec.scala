package com.phasmid.laScala.parser

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class TupleStreamSpec extends FlatSpec with Matchers {
  """"Hello", "World!"""" should "be (String) stream via TupleStream" in {
    val tupleStream = TupleStream[Tuple1[String]](Stream("x",""""Hello"""", """"World!""""))
    println(tupleStream)
    val wts = tupleStream.tuples
    wts.head match {
      case Tuple1(s) => assert(s == "Hello")
    }
    wts.tail.head match {
      case Tuple1(s) => assert(s == "World!")
    }
  }
  """"3,5", "8,13"""" should "be (String,String) stream via TupleStream" in {
    val wWts = TupleStream[(String, String)](Stream("x,y", "3,5", "8,13")).tuples
    wWts.head match {
      case (x, y) => assert(x == "3" && y == "5")
    }
    wWts.tail.head match {
      case (x, y) => assert(x == "8" && y == "13")
    }
  }
  it should "map into (Int,Int) via TupleStream" in {
    val wWts = TupleStream[(String, String)](Stream("x,y", "3,5", "8,13"))
    val iIts = wWts map { case (x, y) => (x.toInt, y.toInt) }
    iIts.tuples.head match {
      case (x, y) => assert(x == 3 && y == 5)
      case _ => fail("no match")
    }
  }
}


