package com.phasmid.laScala.parser

import com.phasmid.laScala.parser.Valuable._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util._

trait MyValue {
  def asValuable[X: Valuable]: Try[X]
}

case class MyExpression(s: String) extends MyValue {
  implicit val pattern: String = ""

  def asValuable[X: Valuable]: Try[X] = Valuable[X].fromString(s)
}

/**
  * @author scalaprof
  */
class ValuableSpec extends AnyFlatSpec with Matchers {
  "1" should "produce 1 for Int" in {
    val x = MyExpression("1")
    x.asValuable[Int] should matchPattern { case Success(1) => }
  }
  it should "produce 1.0 for Double" in {
    val x = MyExpression("1")
    x.asValuable[Double] should matchPattern { case Success(1.0) => }
  }
}
