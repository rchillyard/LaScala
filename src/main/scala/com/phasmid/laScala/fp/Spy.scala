package com.phasmid.laScala.fp

/**
  * This class and its related object provide a quick-and-dirty spy feature for peeking at the values of Scala
  * expressions.
  *
  * In order, to use it with an explicitly defined spyFunc, please see the way it's done in SpySpec.
  *
  * By default, spying is turned off so, if you want to turn it on for a run, you must set spying to true.
  *
  * Created by scalaprof on 8/17/16.
  */
class Spy

object Spy {

  /**
    * This method (and the related class Spy) is here only to enable the implicit spyFunc mechanism.
    * It you declare the return from spyFunc as Unit, the compiler doesn't know where to look for the implicit function.
    * @param x
    * @return
    */
  def apply(x: Unit): Spy = new Spy()

  /**
    * This is the global setting for whether to invoke the spyFunc of each spy invocation. However, each invocation can also turn spying off for itself.
    */
  var spying: Boolean = false

  /**
    * This method is used in development/debug phase to see the values of expressions
    * without altering the flow of the code.
    *
    * @param w a String to be used as the prefix of the resulting message
    * @param x the value being spied on
    * @param b if true AND if spying is true, the spyFunc will be called (defaults to true)
    * @param spyFunc (implicit) the function to be called with a String based on w and x IF b && spying are true
    * @tparam X the type of the value
    * @return the value of x
    */
  def spy[X](w: => String, x: X, b: Boolean = true)(implicit spyFunc: String=>Spy): X = { if (b&&spying) spyFunc(s"$w: $x"); x }

  /**
    * This is the default spy function
    * @param s the message to be output when spying
    */
  implicit def spyFunc(s: String): Spy =  Spy(println("spy: "+s))
}
