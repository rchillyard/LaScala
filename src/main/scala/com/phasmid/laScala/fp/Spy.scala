package com.phasmid.laScala.fp

import java.io.PrintStream

import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

/**
  * This trait and its related object provide a functional-programming style (if somewhat quick-and-dirty) spy feature for logging (or otherwise peeking at) the values of Scala
  * expressions.
  *
  * While this mechanism (by default) logs values using SLF4J, it does so in a functional style.
  * You do not have to break up your functional expressions to write logging statements (unless they are the recursive calls of a tail-recursive method).
  *
  * If you just want to slip in a quick invocation of Spy.spy, you will need to provide an implicit spyFunc.
  * The proper simple way to do this is to declare a logger as follows:
  * implicit val logger = Spy.getLogger(getClass)
  *
  * There's even an (improper) default logger based on the Spy class itself which you can use simply by adding this:
  * import Spy._
  *
  * In order to use it with an explicitly defined spyFunc, please see the way it's done in SpySpec.
  *
  * By default, spying is turned on so, if you want to turn it off for a run, you must set spying to false.
  *
  * Created by scalaprof on 8/17/16.
  */
trait Spy

/**
  * Companion object to Spy.
  */
object Spy {

  /**
    * This method (and the related class Spy) is here only to enable the implicit spyFunc mechanism.
    * It you declare the return from spyFunc as Unit, the compiler doesn't know where to look for the implicit function.
    *
    * @param x ignored
    * @return a new Spy instance
    */
  def apply(x: Unit): Spy = new Spy() {}

  /**
    * This is the global setting for whether to invoke the spyFunc of each spy invocation. However, each invocation can also turn spying off for itself.
    * Normally it is true. Think of this as the red emergency button. Setting this to false turns all spying off everywhere.
    */
  var spying: Boolean = true

  /**
    * This method is used in development/debug phase to "see" the values of expressions
    * without altering the flow of the code.
    *
    * The behavior implied by "see" is defined by the spyFunc. This could be a simple println (which is the default) or a SLF4J debug function or whatever.
    *
    * The caller MUST provide an implicit value in scope for a Logger (unless the spyFunc has been explicitly defined to use some other non-logging mechanism, such as calling getPrintlnSpyFunc).
    *
    * NOTE that there will be times when you cannot use spy, for example when yielding the result of a tail-recursive method. You will need to use log then instead.
    *
    * @param message       a String to be used as the prefix of the resulting message OR as the whole string where "{}" will be substituted by the value.
    *                      Note that the message will only be evaluated if spying will actually occur, otherwise, since it is call-by-name, it will never be evaluated.
    * @param x             the value being spied on and which will be returned by this method.
    * @param b             if true AND if spying is true, the spyFunc will be called (defaults to true). However, note that this is intended only for the situation
    *                      where the default spyFunc is being used. If a logging spyFunc is used, then logging should be turned on/off at the class level via the
    *                      logging configuration file.
    * @param spyFunc       (implicit) the function to be called (as a side-effect) with a String based on w and x IFF b && spying are true.
    * @param isEnabledFunc (implicit) the function to be called to determine if spying is enabled -- by default this will be based on the (implicit) logger.
    * @tparam X the type of the value.
    * @return the value of x.
    */
  def spy[X](message: => String, x: X, b: Boolean = true)(implicit spyFunc: String => Spy, isEnabledFunc: Spy => Boolean): X = {
    if (b && spying && isEnabledFunc(mySpy)) {
      lazy val w = message
      val brackets = "{}"
      val msg = if (w contains brackets)
        w.replace(brackets, s"$x")
      else
        x match {
          case () => w
          // NOTE: If the value to be spied on is a Stream, we arbitrarily show the first 5 items
          // (Be Careful: potential side-effect: if the actual invoker doesn't evaluate as many as 5 items, we will have evaluated more than are necessary)
          case s: Stream[_] => s"$w: [Stream showing at most 5 items] ${s.take(5).toList}"
          // XXX: If the value to be spied on is Success(_) then we invoke spy on the underlying value and capture the message generated
          case Success(z) =>
            val sb = new StringBuilder("")

            // XXX what's all this about?
            implicit def spyFunc(s: String): Spy = Spy(sb.append(s"explicit spy: $s\n"))

            spy(s"$w: Success", z, b)
            sb.toString
          // XXX: If the value to be spied on is Failure(_) then we invoke get the localized message of the cause
          case Failure(t) => s"$w: Failure($t)"
          // XXX: If the value to be spied on is Future(_) then we invoke spy on the underlying value when it is completed
          case f: Future[_] =>
            import scala.concurrent.ExecutionContext.Implicits.global
            f.onComplete(spy("$w: Future", _, b))
            s"$w: to be provided in the future"
          // XXX: If the value to be spied on is a common-or-garden object, then we simply form the appropriate string using the toString method
          case _ => s"$w: $x"
        }
      spyFunc(msg)
    }
    x
  }

  /**
    * This method is used in development/debug phase to "see" a string while returning Unit.
    *
    * It is recommended simply to import Spy._ before invoking this method.
    *
    * CONSIDER renaming this method as debug and providing warn, info, etc. versions
    *
    * @param w       a String to be used as the prefix of the resulting message
    * @param b       if true AND if spying is true, the spyFunc will be called (defaults to true)
    * @param spyFunc (implicit) the function to be called with a String based on w and x IF b && spying are true
    * @return the value of x
    */
  def log(w: => String, b: Boolean = true)(implicit spyFunc: String => Spy, isEnabledFunc: Spy => Boolean) {spy(w, (), b); ()}

  /**
    * This method can be used if you have an expression you would like to be evaluated WITHOUT any spying going on.
    * Bear in mind that this turns off ALL spying during the evaluation of x
    *
    * @param x the expression
    * @tparam X the type of the expression
    * @return the expression
    */
  def noSpy[X](x: => X): X = {
    val safe = spying
    spying = false
    val r = x
    spying = safe
    r
  }

  /**
    * The standard prefix for spy messages: "spy: "
    */
  private val prefix = "spy: "

  /**
    * For really quick-and-dirty spying, you can use this default Logger simply by importing Spy._
    */
  implicit val defaultLogger: Logger = getLogger(getClass)

  /**
    * This is the default spy function
    *
    * @param s the message to be output when spying
    */
  implicit def spyFunc(s: String)(implicit logger: Logger): Spy = Spy(logger.debug(prefix + s))

  /**
    * This is the default isEnabled function
    * This method takes a Spy object (essentially a Unit) and returns a Boolean if the logger is enabled for debug.
    *
    * Note that it is necessary that this method takes a Spy, in the same way that the spyFunc must return a Spy --
    * so that the implicits can be found.
    *
    * @param x      an instance of Spy
    * @param logger an (implicit) logger
    * @return true if the logger is debugEnabled
    */
  implicit def isEnabledFunc(x: Spy)(implicit logger: Logger): Boolean = logger.isDebugEnabled

  /**
    * Convenience method to get a logger for a class.
    * By using this method, the caller does not need to import any logging classes.
    *
    * @param clazz class for which the logger is required
    * @return a Logger
    */
  def getLogger(clazz: Class[_]): Logger = LoggerFactory.getLogger(clazz)

  /**
    * Get a println-type spyFunc that can be made an implicit val at the point of the spy method invocation so that spy invokes println statements instead of logging.
    *
    * @param ps the PrintStream to use (defaults to System.out).
    * @return a spy function
    */
  def getPrintlnSpyFunc(ps: PrintStream = System.out): String => Spy = { s => Spy(ps.println(prefix + s)) }

  private val mySpy = apply(())
}
