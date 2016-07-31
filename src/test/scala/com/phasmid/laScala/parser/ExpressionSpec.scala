package com.phasmid.laScala.parser

import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scala.util._

/**
  * @author scalaprof
  */
class ExpressionSpec extends FlatSpec with Matchers {

  "expression" should """evaluate 1.0K as 1000.0""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, "1.0K")
    implicit def lookup(s: String): Option[Double] = None
    r should matchPattern { case parser.Success(_, _) => }
    r.get.evaluate[Double] should matchPattern { case Success(Right(1000.0)) => }
  }
  it should """evaluate "Hello, World!"""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, """"Hello, World!"""")
    implicit def lookup(s: String): Option[Double] = None
    r should matchPattern { case parser.Success(_, _) => }
    r.get.evaluate[Double] should matchPattern { case Success(Left("Hello, World!")) => }
  }
  it should """not evaluate "Hello, World!"*"Hello, World!"""" in {
    val parser = new RuleParser()
    val r = parser.parseAll(parser.expr, """"Hello, World!"*"Hello, World!"""")
    implicit def lookup(s: String): Option[Double] = None
    r should matchPattern { case parser.Success(_, _) => }
    r.get.evaluate[Double] should matchPattern { case Failure(_) => }
  }
}
