package com.phasmid.laScala

import com.phasmid.laScala.fp.FP

/**
  * This trait is to support Kleenean algebra, potentially in a disjunctive or conjunctive expression.
  * The truth-tables for Kleenean logic can be found here: See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  * Or you can look at KleeneanSpec to see what the rules are.
  */
trait Maybe extends (() => Option[Boolean]) {
  /**
    * Left-associative conjunctive operator with another Maybe
    *
    * @param m other Maybe value
    * @return a Maybe value with is the Kleenean logical AND of this and m
    */
  def :&(m: Maybe) = Kleenean.and(apply, m())

  /**
    * Left-associative disjunctive operator with another Maybe
    *
    * @param m other Maybe value
    * @return a Maybe value with is the Kleenean logical OR of this and m
    */
  def :|(m: Maybe) = Kleenean.or(apply, m())

  /**
    * Left-associative conjunctive operator with an Option[Boolean]
    *
    * @param x other Maybe value
    * @return a Maybe value with is the Kleenean logical AND of this and x
    */
  def :&(x: Option[Boolean]) = Kleenean.and(x, apply)

  /**
    * Left-associative disjunctive operator with an Option[Boolean]
    *
    * @param x other Maybe value
    * @return a Maybe value with is the Kleenean logical OR of this and x
    */
  def :|(x: Option[Boolean]) = Kleenean.or(x, apply)

  /**
    * Right-associative conjunctive operator with an Option[Boolean]
    *
    * @param x other Maybe value
    * @return a Maybe value with is the Kleenean logical AND of this and x
    */
  def &:(x: Option[Boolean]) = Kleenean.and(apply, x)

  /**
    * Right-associative disjunctive operator with an Option[Boolean]
    *
    * @param x other Maybe value
    * @return a Maybe value with is the Kleenean logical OR of this and x
    */
  def |:(x: Option[Boolean]) = Kleenean.or(apply, x)

  /**
    * Left-associative conjunctive operator with a Boolean
    *
    * @param b other Boolean value
    * @return a Maybe value with is the Kleenean logical AND of this and b
    */
  def :&(b: Boolean) = Kleenean.and(Some(b), apply)

  /**
    * Left-associative disjunctive operator with a Boolean
    *
    * @param b other Maybe value
    * @return a Maybe value with is the Kleenean logical OR of this and b
    */
  def :|(b: Boolean) = Kleenean.or(Some(b), apply)

  /**
    * Right-associative conjunctive operator with a Boolean
    *
    * @param b other Maybe value
    * @return a Maybe value with is the Kleenean logical AND of this and b
    */
  def &:(b: Boolean) = Kleenean.and(apply, Some(b))

  /**
    * Right-associative disjunctive operator with a Boolean
    *
    * @param b other Maybe value
    * @return a Maybe value with is the Kleenean logical OR of this and b
    */
  def |:(b: Boolean) = Kleenean.or(apply, Some(b))

  /**
    * Method to convert this Maybe into a Boolean
    * @param default the value to use if this is None
    * @return a Boolean corresponding to either the existing Boolean or else the given default.
    */
  def toBoolean(default: Boolean) = apply().getOrElse(default)

  /**
    * Method to convert this Maybe into an integer corresponding
    * to the return value of compareTo
    * @return either 1 (true), 0 (maybe), or -1 (false)
    */
  def toInt = apply() match {
    case Some(true) => 1
    case Some(false) => -1
    case None => 0
  }

  /**
    * Method to deny (invert, negate, ...) this Maybe
    * @return Some(!x) if exists, else return None
    */
  def deny = Kleenean(apply() map (!_))

  override def toString = apply().toString
}

/**
  * This is the Kleenean case class which extends Maybe
  *
  * @param value the Option[Boolean] value
  */
case class Kleenean(value: Option[Boolean]) extends Maybe {
  def apply(): Option[Boolean] = value
}

/**
  * This case object is the None version of Maybe. It is used for the bookends of a dis/conjunctive expression of Maybes.
  * But be careful, you need to understand the rules of Kleenean logic with regard to None combining with true or false.
  */
object ^^ extends Kleenean(None)

/**
  * Companion object to Maybe
  */
object Maybe {
  def and(x: Option[Boolean], y: => Option[Boolean]): Option[Boolean] = FP.map2(x, y)(_ && _) orElse Maybe.&&&(x) orElse Maybe.&&&(y)

  def or(x: Option[Boolean], y: => Option[Boolean]): Option[Boolean] = FP.map2(x, y)(_ || _) orElse Maybe.|||(x) orElse Maybe.|||(y)

  private def &&&(to: Option[Boolean]): Option[Boolean] = to match {
    case Some(true) => None
    case Some(false) => Some(false)
    case None => None
  }

  private def |||(to: Option[Boolean]): Option[Boolean] = to match {
    case Some(false) => None
    case Some(true) => Some(true)
    case None => None
  }
}

/**
  * Companion object to Kleenean
  */
object Kleenean {
  def apply(x: Boolean): Maybe = Kleenean(Some(x))

  def apply(x: Int): Maybe = x match {
    case 0 => Kleenean(None)
    case _ => Kleenean(x>0)
  }

  def apply(): Maybe = Kleenean(None)

  def and(x: Option[Boolean], y: => Option[Boolean]): Kleenean = Kleenean(Maybe.and(x, y))

  def or(x: Option[Boolean], y: => Option[Boolean]): Kleenean = Kleenean(Maybe.or(x,y))
}