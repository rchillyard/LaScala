/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.fp.RenderableFunction.{asFunctionType, callByValue}
import com.phasmid.laScala.{Prefix, Renderable, RenderableTraversable}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.reflect.ClassTag
import scala.util._

/**
  * This case class represents a "closure" on a renderable function (f) and a set of bound parameters (ps).
  *
  * @param f  the (renderable) function
  * @param ps the parameters, each of which may be a value or a Closure
  * @tparam T the parameter type of the closure
  * @tparam R the result type of the closure
  */
case class Closure[T, R: ClassTag](f: RenderableFunction[R], ps: Parameter[T]*) extends (() => Try[R]) with Renderable {

  private implicit val logger = Spy.getLogger(getClass)

  /**
    * Method to evaluate this closure. If the arity of this is not equal to zero, a Failure will result
    *
    * @return a Try[R]
    */
  def apply(): Try[R] = if (arity == 0) for (g <- partiallyApply; h <- g.f.callByName()) yield h
  else Failure(RenderableFunctionException(s"cannot evaluate this closure (with arity $arity): $this"))

  /**
    * Method to partially apply this Closure, resulting in a RenderableFunction (i.e. a Closure without any parameters).
    *
    * @return a RenderableFunction[R] wrapped in Try.
    */
  def partiallyApply: Try[Closure[_, R]] = for (g <- f.partiallyApplyParameters(ps.toList.asInstanceOf[Seq[Parameter[_]]])) yield Closure(g)

  /**
    * Method to determine if this Closure is fully applied. That's to say we can invoke apply() without
    * having to evaluate any of the parameters (they are all evaluated already).
    *
    * XXX: not currently used
    *
    * @return true if the arity is zero, the function f has no call-by-name parameters and if all of the
    *         parameters for this Closure are themselves evaluated.
    */
  def isFullyApplied: Boolean = arity == 0 && f.alwaysEvaluate && Closure.isParameterSetEvaluated(ps)

  /**
    * Method to bind an additional parameter to this Closure. The resulting Closure will have arity one less than this.
    *
    * XXX: this is currently used only by ClosureSpec
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

  override def toString(): String = render()

  def render(indent: Int)(implicit tab: (Int) => Prefix): String = {
    implicit def renderableTraversable(xs: Traversable[_]): Renderable = RenderableTraversable(xs, " ,)")

    val z = ps.render(indent)
    s"Closure($f," + z
  }
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

  // XXX: not currently used
  private def isFullyApplied[T](tp: Parameter[T]): Boolean = tp match {
    case Left(_) => true
    case Right(c) => c.isFullyApplied
  }

  // XXX: not currently used
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
    case Right(c) => c.arity - 1
  }

  /**
    * Factory method to create a VarArgs Closure from a varargs list of Parameter[T]
    *
    * @param tps the varargs parameters
    * @tparam T the underlying type
    * @return a Closure resulting in Seq[T]
    */
  def createVarArgsClosure[T: ClassTag](tps: Parameter[T]*): Closure[T, Seq[T]] = {

    val (tss: Seq[Seq[T]], _tps: Seq[Parameter[T]]) = VarArgs[T, T, Closure[_, T]](tps, identity).split

    /**
      * Get the number of variables/functions
      */
    val m = _tps.length

    /**
      * Get the total number of parameters
      */
    val n = (tss map (_.size)) sum

    assert(tps.length == n + m, s"${tps.length} should equal $n+$m")
    assert(tss.length == m + 1, s"${tss.length} should equal $m+1")

    val f = m match {
      case 0 =>
        asFunctionType { _: Product => ??(tss.head).get }
      case 1 =>
        asFunctionType { x: Tuple1[T] => (tss.head ++: Some(x._1) +: ??(tss(1))).get }
      case 2 =>
        asFunctionType { x: (T, T) => (tss.head ++: Some(x._1) +: tss(1) ++: Some(x._2) +: ??(tss(2))).get }
      case 3 =>
        logger.warn(s"getVarArgsFunction: used the highest supported value of n ($m)")
        asFunctionType { x: (T, T, T) => (tss.head ++: Some(x._1) +: tss(1) ++: Some(x._2) +: tss(2) ++: Some(x._3) +: ??(tss(3))).get }
      case _ => throw RenderableFunctionException(s"getVarArgsFunction: the maximum number of non-constant args is 3 but $m requested")
    }

    val cs: ParamClasses = Stream.continually(implicitly[ClassTag[T]].asInstanceOf[ClassTag[Any]]) take m
    Closure(RenderableFunction.apply(m, FunctionString("mkList", m), callByValue(m), cs)(f), _tps: _*)
  }


}

case class VarArgs[T, X, Y](xYes: Seq[Either[X, Y]], x2t: X => T) {
  def split: (Seq[Seq[T]], Seq[Either[X, Y]]) = {
    @tailrec
    def inner(result: (Seq[Seq[T]], Seq[Either[X, Y]]), work: Seq[Either[X, Y]]): (Seq[Seq[T]], Seq[Either[X, Y]]) = work match {
      case Nil => result
      case h :: t => h match {
        case Left(x) =>
          inner((result._1.init :+ (result._1.last :+ x2t(x)), result._2), t)
        case Right(_) => inner((result._1 :+ Nil, result._2 :+ h), t)
      }
    }

    inner((Seq(Nil), Nil), xYes.toList)
  }

}

case class ??[T](tso: Option[Seq[T]]) {
  self =>
  /**
    * Method to get the underlying Seq[T]
    *
    * @return a Seq[T] or
    * @throws NoSuchElementException logic error which should never happen
    */
  def get: Seq[T] = tso.get

  def get(i: Int): Option[T] = for (ts <- tso; t <- ts.lift(i)) yield t

  def +:(to: Option[T]): ??[T] = ??(??.+:(to, tso))

  def ++:(tso: Option[Seq[T]]): ??[T] = ??(??.++(tso, self.tso))

  def +:(to: T): ??[T] = +:(Some(to))

  def ++:(tso: Seq[T]): ??[T] = ++:(Some(tso))
}

object ?? {
  def apply[T](ts: Seq[T]): ??[T] = ??(Some(ts))

  def f_++[T](x: Seq[T], y: Seq[T]): Seq[T] = x ++ y

  def f_+:[T](x: T, y: Seq[T]): Seq[T] = x +: y

  def ++[T](x: Option[Seq[T]], y: Option[Seq[T]]): Option[Seq[T]] = FP.map2(x, y)(f_++)

  def +:[T](x: Option[T], y: Option[Seq[T]]): Option[Seq[T]] = FP.map2(x, y)(f_+:)
}


