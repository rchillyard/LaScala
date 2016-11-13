package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util._

/**
  * @author scalaprof
  */
class KleeneanSpec extends FlatSpec with Matchers with Inside {
  behavior of ":|"
  it should "combine properly" in {
    Kleenean() :| false should matchPattern { case Kleenean(None) => }
    Kleenean() :| true should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(true) :| false should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(false) :| false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :| true should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(true) :| true should matchPattern { case Kleenean(Some(true)) => }
    Kleenean() :| Kleenean() should matchPattern { case Kleenean(None) => }
    Kleenean() :| Kleenean(false) should matchPattern { case Kleenean(None) => }
    Kleenean() :| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(true) :| Kleenean(false) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(false) :| Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(true) :| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    ^^ :| Kleenean() should matchPattern { case Kleenean(None) => }
    ^^ :| true should matchPattern { case Kleenean(Some(true)) => }
    ^^ :| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    ^^ :| false should matchPattern { case Kleenean(None) => }
    ^^ :| Kleenean(false) should matchPattern { case Kleenean(None) => }
  }

  behavior of ":&"
  it should "combine properly" in {
    Kleenean() :& false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean() :& true should matchPattern { case Kleenean(None) => }
    Kleenean(true) :& false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :& false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :& true should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(true) :& true should matchPattern { case Kleenean(Some(true)) => }
    Kleenean() :& Kleenean() should matchPattern { case Kleenean(None) => }
    Kleenean() :& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean() :& Kleenean(true) should matchPattern { case Kleenean(None) => }
    Kleenean(true) :& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :& Kleenean(true) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(true) :& Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    ^^ :& Kleenean() should matchPattern { case Kleenean(None) => }
    ^^ :& true should matchPattern { case Kleenean(None) => }
    ^^ :& Kleenean(true) should matchPattern { case Kleenean(None) => }
    ^^ :& false should matchPattern { case Kleenean(Some(false)) => }
    ^^ :& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
  }

  behavior of "^^"
  it should "be None" in {
    ^^.value should matchPattern { case None => }
  }
}
