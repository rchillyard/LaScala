package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util._

/**
  * @author scalaprof
  */
class TrialSpec extends FlatSpec with Matchers with Inside {
  "Lift" should "work" in {
    val toint = Lift[Any, Int] { case x: String => x.toInt }
    toint("10") should matchPattern { case Success(10) => }
    toint("10.0") should matchPattern { case Failure(_) => }
  }
  "none" should "work" in {
    val toint = Trial.none[Any, Int] :^ { case x: String => x.toInt }
    toint("10") should matchPattern { case Success(10) => }
    toint("10.0") should matchPattern { case Failure(_) => }
  }
  "lift" should "work" in {
    val toint = Trial.lift[Any, Int] { case x: String => x.toInt }
    toint("10") should matchPattern { case Success(10) => }
    toint("10.0") should matchPattern { case Failure(_) => }
  }
  "Trial(Lift)" should "be composable using ^: and work in correct order (1)" in {
    val toInt: PartialFunction[Any, Any] = {
      case x: String => x.toInt
    }
    // NOTE that Trial(Lift(f)) is equivalent to LiftTrial(f)
    val trialIntString = toInt ^: Trial(Lift[String, Any] { x: String => x })
    trialIntString("10.0") should matchPattern { case Success("10.0") => }
    trialIntString("10") should matchPattern { case Success(10) => }
  }
  it should "be composable using ^: and work in correct order (2)" in {
    val toString: PartialFunction[Any, Any] = {
      case x: String => x
    }
    // NOTE that Trial.Lift(f) is also equivalent to LiftTrial(f) and so Trial(Lift(f))
    val trialStringInt = toString ^: Trial.lift[String, Any] { x: String => x.toInt }
    trialStringInt("10.0") should matchPattern { case Success("10.0") => }
    trialStringInt("10") should matchPattern { case Success("10") => }
  }
  it should "be composable using ^: (3)" in {
    val toInt: PartialFunction[Any, Any] = {
      case x: String => x.toInt
    }
    val toDouble: PartialFunction[Any, Any] = {
      case x: String => x.toDouble
    }
    val trialIntDoubleString = toInt ^: toDouble ^: Trial(Lift[String, Any] { x: String => x })
    trialIntDoubleString("10.0") should matchPattern { case Success(10.0) => }
    trialIntDoubleString("10") should matchPattern { case Success(10) => }
    trialIntDoubleString("10.0X") should matchPattern { case Success("10.0X") => }
  }
  it should "be composable using :| and work in correct order (1)" in {
    // XXX for convenience we can use LiftMatch here but it's equivalent to Lift({case x:String => x.toInt})
    val toInt = LiftMatch { case x: String => x.toInt }
    val trialStringInt = Trial(Lift[String, Any] { x: String => x }) :| toInt
    trialStringInt("10.0") should matchPattern { case Success("10.0") => }
    trialStringInt("10") should matchPattern { case Success("10") => }
  }
  it should "be composable using :^ and work in correct order (2)" in {
    val toString: PartialFunction[Any, Any] = {
      case x: String => x
    }
    val trialIntString = Trial(Lift[String, Any] { x: String => x.toInt }) :^ toString
    trialIntString("10.0") should matchPattern { case Success("10.0") => }
    trialIntString("10") should matchPattern { case Success(10) => }
  }
  it should "be composable using :^ (3)" in {
    val toInt = { x: Any => x match {
      case x: String => x.toInt
    }
    }
    val toDouble: PartialFunction[Any, Any] = {
      case x: String => x.toDouble
    }
    val toString: PartialFunction[Any, Any] = {
      case x: String => x
    }
    val trialDoubleIntString = LiftTrial[String, Any](toDouble) :^ toInt :^ toString
    trialDoubleIntString("10.0") should matchPattern { case Success(10.0) => }
    trialDoubleIntString("10") should matchPattern { case Success(10.0) => }
    trialDoubleIntString("10.0X") should matchPattern { case Success("10.0X") => }
  }

  "Trial" should "work" in {
    val trialInt = Trial[Any, Int] { case x: String => Try(x.toInt) }
    trialInt("10") should matchPattern { case Success(10) => }
    trialInt("10.0") should matchPattern { case Failure(_) => }
  }
  it should "be composable using |: and work in correct order (1)" in {
    val toInt: PartialFunction[Any, Try[Any]] = {
      case x: String => Try(x.toInt)
    }
    val trialIntString = toInt |: Trial[String, Any] { x: String => Success(x) }
    trialIntString("10.0") should matchPattern { case Success("10.0") => }
    trialIntString("10") should matchPattern { case Success(10) => }
  }
  it should "be composable using |: and work in correct order (2)" in {
    val toString: PartialFunction[Any, Try[Any]] = {
      case x: String => Success(x)
    }
    val trialStringInt = toString |: Trial[String, Any] { x: String => Try(x.toInt) }
    trialStringInt("10.0") should matchPattern { case Success("10.0") => }
    trialStringInt("10") should matchPattern { case Success("10") => }
  }
  it should "be composable using |: (3)" in {
    val toInt: PartialFunction[Any, Try[Any]] = {
      case x: String => Try(x.toInt)
    }
    val toDouble: PartialFunction[Any, Try[Any]] = {
      case x: String => Try(x.toDouble)
    }
    val trialIntDoubleString = toInt |: toDouble |: Trial[String, Any] { x: String => Try(x) }
    trialIntDoubleString("10.0") should matchPattern { case Success(10.0) => }
    trialIntDoubleString("10") should matchPattern { case Success(10) => }
    trialIntDoubleString("10.0X") should matchPattern { case Success("10.0X") => }
  }
  it should "be composable using :| and work in correct order (1)" in {
    // XXX for convenience we can use Match (or LiftMatch) here
    val toInt = LiftMatch { case x: String => x.toInt }
    val trialStringInt = Trial[String, Any] { x: String => Success(x) } :| toInt
    trialStringInt("10.0") should matchPattern { case Success("10.0") => }
    trialStringInt("10") should matchPattern { case Success("10") => }
  }
  it should "be composable using :| and work in correct order (2)" in {
    val toString = Match { case x: String => Success(x) }
    val trialIntString = Trial[String, Any] { x: String => Try(x.toInt) } :| toString
    trialIntString("10.0") should matchPattern { case Success("10.0") => }
    trialIntString("10") should matchPattern { case Success(10) => }
  }
  it should "be composable using :| (3)" in {
    val toInt = Match { case x: String => Try(x.toInt) }
    val toDouble = Match { case x: String => Try(x.toDouble) }
    val toString = Match { case x: String => Success(x) }
    val trialDoubleIntString = Trial[String, Any](toDouble) :| toInt :| toString
    trialDoubleIntString("10.0") should matchPattern { case Success(10.0) => }
    trialDoubleIntString("10") should matchPattern { case Success(10.0) => }
    trialDoubleIntString("10.0X") should matchPattern { case Success("10.0X") => }
  }
  ":|" should "keep the Failures correctly" in {
    def failure1(x: String): Try[String] = Failure(new Exception("1"))
    def failure2(x: String): Try[String] = Failure(new Exception("2"))
    val r = Trial(failure1) :| Trial(failure2)
    val r1 = r("")
    r1 should matchPattern { case Failure(_) => }
    inside(r1) {
      case Failure(x) => x.getLocalizedMessage shouldBe "2"
    }
  }
  "|:" should "keep the Failures correctly" in {
    def failure1(x: String): Try[String] = Failure(new Exception("1"))
    def failure2(x: String): Try[String] = Failure(new Exception("2"))

    // XXX we actually need the place-holders here
    val r = failure1 _ |: failure2 _ |: Trial.none
    val r1 = r("x")
    r1 should matchPattern { case Failure(_) => }
    inside(r1) {
      case Failure(x: NoMatchingTrialException) => x.getLocalizedMessage shouldBe "no matching trial for x"
    }
  }
}
