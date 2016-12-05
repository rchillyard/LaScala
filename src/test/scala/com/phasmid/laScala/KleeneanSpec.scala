package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util._

/**
  * See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  *
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
    Kleenean() :|| Kleenean() should matchPattern { case Kleenean(None) => }
    Kleenean() :|| Kleenean(false) should matchPattern { case Kleenean(None) => }
    Kleenean() :|| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(true) :|| Kleenean(false) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(false) :|| Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :|| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(true) :|| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    ^^ :|| Kleenean() should matchPattern { case Kleenean(None) => }
    ^^ :| true should matchPattern { case Kleenean(Some(true)) => }
    ^^ :|| Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    ^^ :| false should matchPattern { case Kleenean(None) => }
    ^^ :|| Kleenean(false) should matchPattern { case Kleenean(None) => }
  }

  behavior of ":&"
  it should "combine properly" in {
    Kleenean() :& false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean() :& true should matchPattern { case Kleenean(None) => }
    Kleenean(true) :& false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :& false should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :& true should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(true) :& true should matchPattern { case Kleenean(Some(true)) => }
    Kleenean() :&& Kleenean() should matchPattern { case Kleenean(None) => }
    Kleenean() :&& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean() :&& Kleenean(true) should matchPattern { case Kleenean(None) => }
    Kleenean(true) :&& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :&& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false) :&& Kleenean(true) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(true) :&& Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    ^^ :&& Kleenean() should matchPattern { case Kleenean(None) => }
    ^^ :& true should matchPattern { case Kleenean(None) => }
    ^^ :&& Kleenean(true) should matchPattern { case Kleenean(None) => }
    ^^ :& false should matchPattern { case Kleenean(Some(false)) => }
    ^^ :&& Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
  }

  behavior of "|:"
  it should "combine properly" in {
    false |: Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    false |: Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    true |: Kleenean(false) should matchPattern { case Kleenean(Some(true)) => }
    true |: Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    true |: ^^ should matchPattern { case Kleenean(Some(true)) => }
    false |: ^^ should matchPattern { case Kleenean(None) => }
  }

  behavior of "&:"
  it should "combine properly" in {
    false &: Kleenean(true) should matchPattern { case Kleenean(Some(false)) => }
    false &: Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    true &: Kleenean(false) should matchPattern { case Kleenean(Some(false)) => }
    true &: Kleenean(true) should matchPattern { case Kleenean(Some(true)) => }
    true &: ^^ should matchPattern { case Kleenean(None) => }
    false &: ^^ should matchPattern { case Kleenean(Some(false)) => }
  }

  behavior of "^^"
  it should "be None" in {
    ^^() should matchPattern { case None => }
  }

  behavior of "apply(Int)"
  it should "work as expected" in {
    Kleenean(-1) should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(1) should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(0) should matchPattern { case Kleenean(None) => }
  }

  behavior of "deny"
  it should "work as expected" in {
    Kleenean(true).deny should matchPattern { case Kleenean(Some(false)) => }
    Kleenean(false).deny should matchPattern { case Kleenean(Some(true)) => }
    Kleenean(0).deny should matchPattern { case Kleenean(None) => }
  }
}
