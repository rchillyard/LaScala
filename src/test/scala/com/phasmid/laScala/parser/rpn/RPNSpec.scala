package com.phasmid.laScala.parser.rpn

import com.phasmid.laScala.{FP, Trial}
import org.scalatest.{FlatSpec, Matchers}

import scala.util._

class RPNSpec extends FlatSpec with Matchers {
  import com.phasmid.laScala.parser.Valuable.ValuableInt
  "Empty" should "fail on empty" in {
    val rpn: RPN[Int] = RPN()
    rpn.evaluate() should matchPattern {case Failure(_) => }
  }
  "Constant" should "evaluate 1 correctly" in {
    val const = Success(1)
    val rpn: RPN[Int] = RPN().push(Constant({() => const}))
    rpn.evaluate() should matchPattern {case `const` => }
  }
  it should "evaluate $K correctly" in {
    val lookupTable = Map[String,Int]("K"->1000)
    implicit def lookup(s: String): Option[Int] = lookupTable.get(s)
    val rpn = RPN[Int](List("$K"))
    rpn.evaluate() should matchPattern {case Success(1000) => }
  }
  "Number" should "evaluate 1 correctly" in {
    val rpn: RPN[Int] = RPN().push(Number("1"))
    rpn.evaluate() should matchPattern {case Success(1) => }
  }
  "Monadic" should "evaluate 1 chs correctly" in {
    val n: RPN[Int] = RPN().push(Number("1"))
    val rpn: RPN[Int] = n.push(Monadic(x => Try(-x)))
    rpn.evaluate() should matchPattern {case Success(-1) => }
  }
  "Dyadic plus" should "evaluate 1 1 + correctly" in {
    val n: RPN[Int] = RPN().push(Number("1")).push(Number("1"))
    val rpn: RPN[Int] = n.push(Dyadic((x,y) => Try(x+y)))
    rpn.evaluate() should matchPattern {case Success(2) => }
  }
  it should "evaluate 2 1 - correctly" in {
    val n: RPN[Int] = RPN().push(Number("2")).push(Number("1"))
    val rpn: RPN[Int] = n.push(Dyadic((x,y) => Try(x-y)))
    rpn.evaluate() should matchPattern {case Success(1) => }
  }
  it should "evaluate 4 2 / correctly" in {
    val n: RPN[Int] = RPN().push(Number("4")).push(Number("2"))
    val rpn: RPN[Int] = n.push(Dyadic((x,y) => Try(x/y)))
    rpn.evaluate() should matchPattern {case Success(2) => }
  }
  it should "evaluate 5 1 2 + 4 * + 3 - correctly" in {
    // XXX why do we need to create this lookup method when there is a default defined.
    // I believe it's because there are really two implicit values passed in to the RPN.apply method and if
    // we define one, we must define the other.
    val lookupTable = Map[String,Int]()
    implicit def lookup(s: String): Option[Int] = lookupTable.get(s)
    val rpn: RPN[Int] = RPN[Int]("5 1 2 + 4 * + 3 -")
    rpn.evaluate() should matchPattern {case Success(14) => }
  }
}