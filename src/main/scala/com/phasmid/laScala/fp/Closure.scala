/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import org.slf4j.LoggerFactory

import scala.language.postfixOps
import scala.util._

/**
  * This case class represents a "closure" on a renderable function (f) and a set of bound parameters (ps).
  *
  * @param f  the function
  * @param ps the parameters, each of which may be a value or a Closure
  * @tparam T the parameter type of the closure
  * @tparam R the result type of the closure
  */
case class Closure[T, R](f: RenderableFunction[R], ps: Parameter[T]*) extends (() => Try[R]) {

  /**
    * Method to evaluate this closure. If the arity of this is not equal to zero, a Failure will result
    *
    * @return a Try[R]
    */
  def apply(): Try[R] = for (g <- partiallyApply(true); h <- g.f.callByName()) yield h

  /**
    * Method to partially apply this closure.
    *
    * @param evaluateAll (defaults to false) if this is true, then we force all parameters to be evaluated, even if they are call-by-name
    * @return a Closure[R] wrapped in Try. The ps parameter of the result will be empty.
    */
  def partiallyApply(evaluateAll: Boolean = false): Try[Closure[T, R]] = for ((g: RenderableFunction[R], xs) <- f.applyParameters[T](ps.toList, evaluateAll); z = xs map (Right(_))) yield Closure[T, R](g, z: _*)

  /**
    * Method to bind an additional parameter to this Closure. The resulting Closure will have arity one less than this.
    *
    * @param p the parameter which will be inserted immediately before the ith bound parameter.
    * @param i the index at which the new parameter should be inserted (defaults to the number of current parameters,
    *          which is to say, if index is not specified, the new parameter will be appended to the list).
    * @return a new Closure with the same function but with the expanded list
    */
  def bind(p: Parameter[T], i: Int = ps.size): Closure[T, R] = Closure(f, ps :+ p: _*)

  /**
    * The arity of this Closure.
    */
  lazy val arity: Int = ps.foldLeft(f.arity)(_ + Closure.parameterArity(_))

  override def toString(): String = s"Closure($f, $ps)"
}



/**
  * Companion object to Closure
  */
object Closure {
  private implicit val logger = LoggerFactory.getLogger(getClass)

  def logDebug[R](w: String, ry: => Try[R]): Try[R] = ry match {
    case Success(r) => logger.debug(s"$w: $r"); ry
    case Failure(x) => logger.debug(s"$w: failed: $x"); ry
  }

  private def parameterArity[T](p: Parameter[T]): Int = p match {
    case Left(_) => -1
    case Right(c) => c.arity
  }

  /**
    * Factory method to create a VarArgs Closure
    *
    * @param ts the varargs parameters
    * @tparam T the underlying type
    * @return a Closure resulting in Seq[T]
    */
  def createVarArgsClosure[T](ts: T*): Closure[T, Seq[T]] = apply(RenderableFunction.varargs(ts.size), ts map (Left(_)): _*)
}

