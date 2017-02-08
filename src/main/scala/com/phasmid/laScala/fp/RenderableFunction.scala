/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.Tuple0
import com.phasmid.laScala.{Prefix, Renderable}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util._

import Spy._

/**
  * A representation of a function call.
  * The function (func) which defines the function call is of form Product=>R where the product (tuple)
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param arity the number of input parameters that are required to fully apply the given function (func)
  * @param func  the function itself
  * @param w     a human-readable representation of the function
  * @tparam R the ultimate return type of this RenderableFunction
  */
case class RenderableFunction[R](arity: Int, func: (Product) => R, w: FunctionString) extends (Product => R) with Renderable {

  require(w.arity == arity, s"arity $arity is not consistent with $w")

  override def apply(p: Product): R =
    if (arity == p.productArity) func.apply(p)
    else throw RenderableFunctionException(s"cannot apply RenderableFunction: ($this) to a Product of arity ${p.productArity}: $p")

  /**
    * If arity=0, we can rewrite this as a function of Unit=>R
    *
    * @return a function which takes no parameters
    */
  def untupled: (() => R) = if (arity == 0) { () => func(Tuple0) } else
    throw RenderableFunctionException(s"untupled only applies when arity=0")

  /**
    * Method which will take the given RenderableFunction and apply all of the Scalar terms that were parsed,
    * resulting in a RenderableFunction whose one parameter is the Tuple0.
    *
    * @param xs the list of parameters
    * @tparam T the underlying type of the parameters
    * @return a new RenderableFunction (wrapped in Try), whose single input parameter is the Tuple0
    */
  def applyParameters[T](xs: List[Parameter[T]]): Try[RenderableFunction[R]] = {
    @tailrec
    def inner1(r: RenderableFunction[R], work: List[Parameter[T]]): RenderableFunction[R] =
      work match {
        case Nil => r
        case h :: t =>
          h match {
            case Left(s) => // T
              inner1(r.partiallyApply(s), t)
            case Right(f) => // RenderableFunction[T]
              println(s"attempting to evaluate $f for $r with available parameters: $t")
//              if (f.arity >t.size)
//                throw RenderableFunctionException(s"insufficient parameters (${t.size}) for $f")
//              else {
                val (x,y) = t splitAt f.arity
                f.applyParameters(x) match {
                  case Success(g: RenderableFunction[T]) =>
                    println(s"result of ($f).applyParameters($x) is g: $g")
                    if (g.arity==0) {
                      println(s"arity of g == 0; applying untupled to give ${g.untupled()}")
                      inner1(r.partiallyApply(g.untupled()), y)
                    }
                    else {
                      assert(y.isEmpty,"y empty")
                        println(s"arity of g: ${g.arity}; passing g to inner1 and thus to result")
                        inner1(r.partiallyApplyFunction(g), Nil)
                      }
                  case Failure(e) => throw e
                }
//               }
          }
      }

    Try(inner1(this, xs))
  }

  def partiallyApply[T](t: T): RenderableFunction[R] = RenderableFunction.partiallyApply(arity, func, t, w)

  def partiallyApplyFunction[T](f: RenderableFunction[T]): RenderableFunction[R] = RenderableFunction.partiallyApplyFunction(arity, func, w, f)

  override def toString = s"RenderableFunction: arity: $arity, func: $w"

  /**
    * Invert the first n parameter positions of this RenderableFunction
    *
    * @param n the number of parameters to invert
    * @tparam T the underling type of the parameters
    * @return a new RenderableFunction with the parameters inverted
    */
  def invert[T](n: Int): RenderableFunction[R] = RenderableFunction.invert(n, arity, func, w)

  /**
    * Method to render this object in a human-legible manner.
    *
    * @param indent the number of "tabs" before output should start on a new line.
    * @param tab    an implicit function to translate the tab number (i.e. indent) to a String of white space.
    *               Typically (and by default) this will be uniform. But you're free to set up a series of tabs
    *               like on an old typewriter where the spacing is non-uniform.
    * @return a String that, if it has embedded newlines, will follow each newline with (possibly empty) white space,
    *         which is then followed by some human-legible part of *this*.
    */
  def render(indent: Int)(implicit tab: (Int) => Prefix): String = tab(indent) + w.toString
}

case class FreeParam(p: Char) {
  override def toString: String = p.toString + FreeParam.query
}

case class Param(p: Either[String, FreeParam]) {
  def isFree: Boolean = p.isRight

  override def toString: String = "(" + (p match {
    case Left(s) => s
    case Right(_p) => _p.toString
  }) + ")"
}

case class FunctionString(f: String, ps: List[Param]) {
  def invert(n: Int): FunctionString = {
    @tailrec def inner(r1: List[Param], r2: List[Param], i: Int, _ps: List[Param]): List[Param] = _ps match {
      case Nil => r1 ++ r2
      case h :: t => if (i < n) inner(h +: r1, r2, i + 1, t)
      else inner(r1, r2 :+ h, i + 1, t)
    }

    FunctionString(f, inner(Nil, Nil, 0, ps))
  }

  override def toString: String = f + ps.mkString("", "", "")

  def partiallyApply[T](t: => T, index: Int = 0): FunctionString = if (arity > index) {
    val w = java.util.regex.Matcher.quoteReplacement(t match {
      case _: String => s""""$t""""
      case _ => s"$t"
    })
    Spy.log(s"partiallyApply: $t to $f has resulted in $w bound at index $index")
    FunctionString(f, bind(index, w))
  }
  else throw RenderableFunctionException("FunctionString.partiallyApply cannot apply where arity <= 0")

  def arity: Int = free size

  private def free = ps filter (_.isFree)

  def bind(index: Int, w: String): List[Param] = bind(Iterator.continually(None).take(index).toList :+ Some(w))

  /**
    * Method to bind some String values to free variables in this FunctionString.
    *
    * @param wos a sequence of optional Strings
    * @return a sequence of Param values where each param is a copy of the corresponding value in ps (the "original"), *unless*
    *         BOTH the original is free AND the optional String is Some(_)
    */
  def bind(wos: List[Option[String]]): List[Param] = {
    @tailrec def inner(result: List[Param], _ps: List[Param], _ws: List[Option[String]]): List[Param] = _ps match {
      case Nil => result
      case p :: __ps => p match {
        case Param(Left(_)) => inner(result :+ p, __ps, _ws)
        case Param(Right(_)) => _ws match {
          case Nil => inner(result :+ p, __ps, _ws)
          case None :: __ws => inner(result :+ p, __ps, __ws)
          case Some(s) :: __ws => inner(result :+ Param(s"""$s"""), __ps, __ws)
        }
      }
    }

    inner(Nil, ps, wos)
  }
}

object Param {
  def apply(s: String): Param = Param(Left(s))

  def apply(p: FreeParam): Param = Param(Right(p))
}

object FreeParam {
  private val query = """?"""
}

object FunctionString {
  private val params = "abcdefghijklmnopqrstuvwxyz".toList

  def stream(list: List[Char]): Stream[FreeParam] = Stream.cons(FreeParam(list.head), stream(list.tail))

  def apply(f: String, n: Int): FunctionString = new FunctionString(f, stream(params) take n map (Param(_)) toList)
}

/**
  * Companion object to RenderableFunction
  */
object RenderableFunction {
  def apply[R](arity: Int, func: (Product) => R, w: String): RenderableFunction[R] = RenderableFunction(arity, func, FunctionString(w, arity))

  def apply[T, R](f: T => R, w: String): RenderableFunction[R] = apply(1, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T) => R, w: String): RenderableFunction[R] = apply(2, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T, T) => R, w: String): RenderableFunction[R] = apply(3, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T, T, T) => R, w: String): RenderableFunction[R] = apply(4, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T, T, T, T) => R, w: String): RenderableFunction[R] = apply(5, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T, T, T, T, T) => R, w: String): RenderableFunction[R] = apply(6, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T, T, T, T, T, T) => R, w: String): RenderableFunction[R] = apply(7, asTupledFunctionType(f), w)

  def apply[T, R](f: (T, T, T, T, T, T, T, T) => R, w: String): RenderableFunction[R] = apply(8, asTupledFunctionType(f), w)

  def asFunctionType[R](f: Function[_, R]): Product => R = cast[Product => R](f)

  def asTupledFunctionType[T, R](f: T => R): Product => R = asFunctionType[R]({ x: Tuple1[T] => f(x._1) })

  def asTupledFunctionType[T, R](f: (T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def asTupledFunctionType[T, R](f: (T, T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def asTupledFunctionType[T, R](f: (T, T, T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def asTupledFunctionType[T, R](f: (T, T, T, T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T, T, T) => R): Product => R = asFunctionType[R](f.tupled)

  def untupled[T, R](f: Tuple1[T] => R): T => R = { x => f(Tuple1(x)) }

  private def cast[R](x: Any)(implicit z: ClassTag[R]): R = x match {
    case r: R => r

    case _ => throw new ClassCastException(s"x: $x of type ${x.getClass} cannot be cast to type $z")
  }

  private def partiallyApply[T, R](n: Int, f: (Product) => R, t: T, w: FunctionString) = {
    n match {
      case 0 => throw RenderableFunctionException("cannot partially apply a RenderableFunction of arity 0")
      case 1 =>
        val f1: Tuple1[T] => R = f
        val g0 = { _: Product => f1(Tuple1(t)) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g0, s)
      case 2 =>
        val f2 = Function.untupled[T, T, R](f)
        val g1 = RenderableFunction.asFunctionType { x: Tuple1[T] => f2.curried(t)(x._1) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g1, s)
      case 3 =>
        val f3 = Function.untupled[T, T, T, R](f)
        val g2 = RenderableFunction.asFunctionType { x: (T, T) => f3.curried(t)(x._1)(x._2) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g2, s)
      case 4 =>
        val f4 = Function.untupled[T, T, T, T, R](f)
        val g3 = RenderableFunction.asFunctionType { x: (T, T, T) => f4.curried(t)(x._1)(x._2)(x._3) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g3, s)
      case 5 =>
        val f5 = Function.untupled[T, T, T, T, T, R](f)
        val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f5.curried(t)(x._1)(x._2)(x._3)(x._4) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g4, s)
      case 6 =>
        val f6 = FP.untupled[T, T, T, T, T, T, R](f)
        val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f6.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g5, s)
      case 7 =>
        val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
        val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
        val s = w.partiallyApply(t)
        RenderableFunction(n - 1, g6, s)
      case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
    }
  }

  private def partiallyApplyFunction[S, T, R](n: Int, f: (Product) => R, w: FunctionString, g: RenderableFunction[T]): RenderableFunction[R] = {
    g.arity match {
      case 0 => throw RenderableFunctionException("cannot partially apply a RenderableFunction with g having arity 0")
      case 1 =>
        val f1: Tuple1[T] => R = f
        val g1: Tuple1[S] => T = g.func
        val g2 = RenderableFunction.asFunctionType { x: Tuple1[S] => f1(Tuple1(g1(Tuple1(x._1)))) }
        RenderableFunction(g.arity, g2, g.w)
//      case 2 =>
//        val f2 = Function.untupled[T, T, R](f)
//        val g1 = RenderableFunction.asFunctionType { x: Tuple1[T] => f2.curried(t)(x._1) }
//        val s = w.partiallyApply(t)
//        RenderableFunction(n - 1, g1, s)
//      case 3 =>
//        val f3 = Function.untupled[T, T, T, R](f)
//        val g2 = RenderableFunction.asFunctionType { x: (T, T) => f3.curried(t)(x._1)(x._2) }
//        val s = w.partiallyApply(t)
//        RenderableFunction(n - 1, g2, s)
//      case 4 =>
//        val f4 = Function.untupled[T, T, T, T, R](f)
//        val g3 = RenderableFunction.asFunctionType { x: (T, T, T) => f4.curried(t)(x._1)(x._2)(x._3) }
//        val s = w.partiallyApply(t)
//        RenderableFunction(n - 1, g3, s)
//      case 5 =>
//        val f5 = Function.untupled[T, T, T, T, T, R](f)
//        val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f5.curried(t)(x._1)(x._2)(x._3)(x._4) }
//        val s = w.partiallyApply(t)
//        RenderableFunction(n - 1, g4, s)
//      case 6 =>
//        val f6 = FP.untupled[T, T, T, T, T, T, R](f)
//        val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f6.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5) }
//        val s = w.partiallyApply(t)
//        RenderableFunction(n - 1, g5, s)
//      case 7 =>
//        val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
//        val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
//        val s = w.partiallyApply(t)
//        RenderableFunction(n - 1, g6, s)
      case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
    }
  }

  private def invert[T, R](n: Int, arity: Int, f: (Product) => R, w: FunctionString) = {
    if (arity < 2 || n > arity)
      throw RenderableFunctionException(s"can't invert $n parameters of a RenderableFunction of arity $arity")
    else
      arity match {
        case 2 =>
          val f2 = FP.untupled[T, T, R](f).curried
          val f2i = n match {
            case 2 => FP.invert2(f2)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g2 = RenderableFunction.asFunctionType { x: (T, T) => f2i(x._1)(x._2) }
          RenderableFunction(n, g2, w.invert(n))
        case 3 =>
          val f3 = FP.untupled[T, T, T, R](f).curried
          val f3i = n match {
            case 2 => FP.invert2(f3)
            case 3 => FP.invert3(f3)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g3 = RenderableFunction.asFunctionType { x: (T, T, T) => f3i(x._1)(x._2)(x._3) }
          RenderableFunction(n, g3, w.invert(n))
        case 4 =>
          val f4 = FP.untupled[T, T, T, T, R](f).curried
          val f4i = n match {
            case 2 => FP.invert2(f4)
            case 3 => FP.invert3(f4)
            case 4 => FP.invert4(f4)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f4i(x._1)(x._2)(x._3)(x._4) }
          RenderableFunction(n, g4, w.invert(n))
        case 5 =>
          val f5 = FP.untupled[T, T, T, T, T, R](f).curried
          val f5ci = n match {
            case 2 => FP.invert2(f5)
            case 3 => FP.invert3(f5)
            case 4 => FP.invert4(f5)
            case 5 => FP.invert5(f5)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f5ci(x._1)(x._2)(x._3)(x._4)(x._5) }
          RenderableFunction(n, g5, w.invert(n))
        case 6 =>
          val f6 = FP.untupled[T, T, T, T, T, T, R](f).curried
          val f6i = n match {
            case 2 => FP.invert2(f6)
            case 3 => FP.invert3(f6)
            case 4 => FP.invert4(f6)
            case 5 => FP.invert5(f6)
            case 6 => FP.invert6(f6)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f6i(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
          RenderableFunction(n, g6, w.invert(n))
        case _ => throw RenderableFunctionException(s"invert with arity $arity is not supported")
      }
  }
}

/**
  * RenderableFunctionException
  *
  * @param s the message
  */
case class RenderableFunctionException(s: String) extends Exception(s)

