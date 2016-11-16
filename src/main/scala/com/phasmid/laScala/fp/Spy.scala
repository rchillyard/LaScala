package com.phasmid.laScala.fp

/**
  * This class and its related object provide a quick-and-dirty spy feature for looking at the values of Scala
  * expression.
  *
  * In order, to use it with an explicitly defined spyFunc, please see the way it's done in SpySpec.
  *
  * By default, spying is turned off so, if you want to turn it on for a run, you must set spying to true.
  *
  * Created by scalaprof on 11/16/16.
  */
class Spy

object Spy {

  def apply(x: Unit): Spy = new Spy()

  var spying: Boolean = false

  /**
    * This method is used in development/debug phase to see the values of expressions
    * without altering the flow of the code.
    *
    * @param w a String to be used as the prefix of the resulting message
    * @param x the value being spied on
    * @tparam X the type of the value
    * @return the value
    */
  def spy[X](w: => String, x: X)(implicit spyFunc: String=>Spy): X = {if (spying) spyFunc(s"$w: $x"); x}

  /**
    * This is the default spy function
    * @param s the message to be output when spying
    */
  implicit def spyFunc(s: String): Spy =  Spy(println("spy: "+s))
}
