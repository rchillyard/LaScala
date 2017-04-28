package com.phasmid.laScala.parser.rpn

import com.phasmid.laScala.parser.RuleParser
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scala.util._

class RPNSpec extends FlatSpec with Matchers {

  import com.phasmid.laScala.parser.Valuable.ValuableInt

  "Empty" should "fail on empty" in {
    val rpn: RPN[Int] = RPN()
    rpn.evaluate should matchPattern { case Failure(_) => }
  }
  "Constant" should "evaluate 1 correctly" in {
    val const = Success(1)
    val rpn: RPN[Int] = RPN().push(Constant(const))
    rpn.evaluate should matchPattern { case `const` => }
  }
  it should "evaluate $K correctly" in {
    val lookupTable = Map[String, Int]("K" -> 1000)

    implicit def lookup(s: String): Option[Int] = lookupTable.get(s)

    val rpn = RPN[Int](List("$K"))
    rpn.evaluate should matchPattern { case Success(1000) => }
  }
  "Number" should "evaluate 1 correctly" in {
    val rpn: RPN[Int] = RPN().push(Number("1"))
    rpn.evaluate should matchPattern { case Success(1) => }
  }
  "Monadic" should "evaluate negation correctly" in {
    val n: RPN[Int] = RPN().push(Number("1"))
    val rpn: RPN[Int] = n.push(Monadic(x => Try(-x)))
    rpn.evaluate should matchPattern { case Success(-1) => }
  }
  it should "evaluate 1 chs correctly" in {
    import RPN.lookup
    val rpn: RPN[Int] = RPN("1 chs")
    rpn.evaluate should matchPattern { case Success(-1) => }
  }
  it should "evaluate 1 exp correctly" in {
    import RPN.lookup
    val rpn: RPN[Double] = RPN("1 exp")
    rpn.evaluate should matchPattern { case Success(math.E) => }
  }
  "Dyadic plus" should "evaluate 1 1 + correctly" in {
    val n: RPN[Int] = RPN().push(Number("1")).push(Number("1"))
    val rpn: RPN[Int] = n.push(Dyadic((x, y) => Try(x + y)))
    rpn.evaluate should matchPattern { case Success(2) => }
  }
  it should "evaluate 2 1 - correctly" in {
    val n: RPN[Int] = RPN().push(Number("2")).push(Number("1"))
    val rpn: RPN[Int] = n.push(Dyadic((x, y) => Try(x - y)))
    rpn.evaluate should matchPattern { case Success(1) => }
  }
  it should "evaluate 4 2 / correctly" in {
    val n: RPN[Int] = RPN().push(Number("4")).push(Number("2"))
    val rpn: RPN[Int] = n.push(Dyadic((x, y) => Try(x / y)))
    rpn.evaluate should matchPattern { case Success(2) => }
  }
  it should "evaluate 5 1 2 + 4 * + 3 - correctly" in {
    // CONSIDER why do we need to create this lookup method when there is a default defined.
    // ... I believe it's because there are really two implicit values passed in to the RPN.apply method and if
    // ... we define one, we must define the other.
    val lookupTable = Map[String, Int]()

    implicit def lookup(s: String): Option[Int] = lookupTable.get(s)

    val rpn: RPN[Int] = RPN[Int]("5 1 2 + 4 * + 3 -")
    rpn.evaluate should matchPattern { case Success(14) => }
  }
  it should "evaluate 3 2 pow correctly (Int)" in {
    import RPN.lookup
    val rpn: RPN[Int] = RPN[Int]("3 2 pow")
    rpn.evaluate should matchPattern { case Success(9) => }
  }
  it should "evaluate 3 2 pow correctly (Double)" in {
    import RPN.lookup
    val rpn: RPN[Double] = RPN[Double]("3 2 pow")
    rpn.evaluate should matchPattern { case Success(9.0) => }
  }
  it should "evaluate pi correctly" in {
    import RPN.lookup
    val rpn: RPN[Double] = RPN[Double]("pi")
    rpn.evaluate should matchPattern { case Success(math.Pi) => }
  }
  it should "evaluate $x correctly" in {
    val lookupTable = Map[String, Int]("x" -> 10)

    implicit def lookup(s: String): Option[Int] = lookupTable.get(s)

    val rpn: RPN[Int] = RPN[Int]("$x")
    rpn.evaluate should matchPattern { case Success(10) => }
  }
  "parseExpression" should """parse 1 as List("1")""" in {
    val parser = new RuleParser()
    val r = parser.parseExpression("1")
    r should matchPattern { case Success(_) => }
    r.get.toRPN shouldBe List("1")
  }
  it should "parse 1B as ..." in {
    val parser = new RuleParser()
    val et = parser.parseExpression("1B")
    et map {
      _.toRPN
    } should matchPattern { case Success(List("1", "1000", "*", "1000", "*", "1000", "*")) => }
  }
  it should "evaluate 1B as 1000000000" in {
    val parser = new RuleParser()
    val et = parser.parseExpression("1B")

    implicit def lookup(s: String): Option[Int] = None

    val xt = for (e <- et; r = e.toRPN; x <- RPN(r).evaluate) yield x
    xt should matchPattern { case Success(1000000000) => }
  }
  it should """evaluate "sharpeRatio"""" in {
    val parser = new RuleParser()
    val et = parser.parseExpression("$sharpeRatio")
    val properties = Map[String, Double]("sharpeRatio" -> 1.0)
    implicit val lookup: String => Option[Double] = properties.get
    val xt: Try[Double] = for (e <- et; r = e.toRPN; x <- RPN[Double](r).evaluate) yield x
    xt should matchPattern { case Success(1.0) => }
  }

}