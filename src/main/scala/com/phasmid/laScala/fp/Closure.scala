/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.reflect.ClassTag
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

  private implicit val logger = Spy.getLogger(getClass)

  /**
    * Method to evaluate this closure. If the arity of this is not equal to zero, a Failure will result
    *
    * @return a Try[R]
    */
  def apply(): Try[R] = if (arity==0)
    for (g <- Spy.spy("g", partiallyApply); h <- Spy.spy("h", g.callByName())) yield h
  else Failure(RenderableFunctionException(s"cannot evaluate this closure (with arity $arity): $this"))

  /**
    * Method to partially apply this Closure, resulting in a RenderableFunction (i.e. a Closure without any parameters).
    *
    * @return a RenderableFunction[R] wrapped in Try.
    */
  def partiallyApply: Try[RenderableFunction[R]] = for (g <- f.partiallyApplyParameters(ps.toList.asInstanceOf[Seq[Parameter[_]]])) yield g

  /**
    * Method to determine if this Closure is fully applied. That's to say we can invoke apply() without
    * having to evaluate any of the parameters (they are all evaluated already).
    *
    * @return true if the arity is zero, the function f has no call-by-name parameters and if all of the
    *         parameters for this Closure are themselves evaluated.
    */
  def isFullyApplied: Boolean = arity == 0 && f.alwaysEvaluate && Closure.isParameterSetEvaluated(ps)

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

  private def isFullyApplied[T](tp: Parameter[T]): Boolean = tp match {
    case Left(_) => true
    case Right(c) => c.isFullyApplied
  }

  private def isParameterSetEvaluated[T](ps: Seq[Parameter[T]]): Boolean = {
    @tailrec
    def inner(r: Boolean, tps: Seq[Parameter[T]]): Boolean =
      if (!r) false else
        tps match {
          case Nil => r
          case tp :: _tps => inner(Closure.isFullyApplied(tp), _tps)
        }

    inner(r = true, ps)
  }

  private def parameterArity[T](p: Parameter[T]): Int = p match {
    case Left(_) => -1
    case Right(c) => c.arity-1
  }

  /**
    * Factory method to create a VarArgs Closure
    *
    * @param ts the varargs parameters
    * @tparam T the underlying type
    * @return a Closure resulting in Seq[T]
    */
  def createVarArgsClosure[T: ClassTag](ts: T*): Closure[T, Seq[T]] = apply(RenderableFunction.varargs(ts.size), ts map (Left(_)): _*)
}

