package com.phasmid.laScala.parser

import com.phasmid.laScala.parser.Valuable._
import org.scalatest.{FlatSpec, Matchers}

import scala.util._

trait MyValue {
  def asValuable[X: Valuable]: Try[X]
}

case class MyExpression(s: String) extends MyValue {
  implicit val pattern = ""
  def asValuable[X: Valuable]: Try[X] = implicitly[Valuable[X]].fromString(s)
}

/**
  * @author scalaprof
  */
class ValuableSpec extends FlatSpec with Matchers {
  "1" should "produce 1 for Int" in {
    val x = MyExpression("1")
    x.asValuable[Int] should matchPattern { case Success(1) => }
  }
  it should "produce 1.0 for Double" in {
    val x = MyExpression("1")
    x.asValuable[Double] should matchPattern { case Success(1.0) => }
  }
}
