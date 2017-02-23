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

/**
  * This case class represents a "closure" on a renderable function (f) and a set of bound parameters (ps).
  *
  * @param f  the function
  * @param ps the parameters, each of which may be a value or a Closure
  * @tparam T the parameter type of the closure
  * @tparam R the result type of the closure
  */
case class Closure[T:ClassTag,+R](f: RenderableFunction[R], ps: Parameter[T]*) extends (() => Try[R]) {

  /**
    * Method to evaluate this closure. If the arity of this is not equal to zero, a Failure will result
    *
    * @return a Try[R]
    */
  def apply(): Try[R] = for (g <- partiallyApply; h <- g.f.callByName()) yield h

  /**
    * Method to partially apply this closure.
    *
    * @return a Closure[R] wrapped in Try. The ps parameter of the result will be empty.
    */
  def partiallyApply: Try[Closure[T, R]] = for (g <- f.applyParameters[T](ps.toList)) yield Closure(g)

  /**
    * Method to bind an additional parameter to this Closure. The resulting Closure will have arity one less than this.
    *
    * @param p the parameter which will be inserted immediately before the ith bound parameter.
    * @param i the index at which the new parameter should be inserted (defaults to the number of current parameters,
    *          which is to say, if index is not specified, the new parameter will be appended to the list).
    * @return a new Closure with the same function but with the expanded list
    */
  def bind(p: Parameter[T], i: Int = ps.size): Closure[T,R] = Closure(f, ps :+ p: _*)

  /**
    * The arity of this Closure.
    */
  lazy val arity: Int = ps.foldLeft(f.arity)(_ + Closure.parameterArity(_))

  override def toString(): String = s"Closure($f, $ps)"
}

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
case class RenderableFunction[+R](arity: Int, func: Product => R, w: FunctionString)(implicit rc: ClassTag[R]) extends (Product => Try[R]) with Renderable {

  require(func != null, s"func is null")
  require(w != null, s"w is null")
  require(w.arity == arity, s"arity $arity is not consistent with $w")

  implicit private val logger = Spy.getLogger(getClass)
  Spy.log(s"created $this with return type: $rc")

  private val returnIsTry = rc.runtimeClass == classOf[Try[_]]

  /**
    * Apply parameter p to this function.
    *
    * NOTE that if R is Try[S], then the result of apply will be R, not Try[R]
    *
    * @param p a Tuple
    * @return an R wrapped in Try
    */
  def apply(p: Product): Try[R] =
  // CONSIDER rewriting the logic here
    if (arity == p.productArity)
      if (returnIsTry)
        func.apply(p).asInstanceOf[Try[R]]
      else
        Try(func.apply(p))
    else
      Failure(RenderableFunctionException(s"cannot apply RenderableFunction: ($this) to a Product of arity ${p.productArity}: $p"))

  /**
    * Apply parameter p to this function.
    *
    * @param t a T
    * @return the result of invoking apply(Tuple1(t))
    */
  def apply[T](t: T): Try[R] = apply(Tuple1(t))

  /**
    * Method to call this RenderableFunction by name only, that's to say without any parameters.
    * If this is a 0-arity function, then Success(r) will be returned. Otherwise, you will get a failure.
    *
    * @return
    */
  def callByName(): Try[R] = apply(Tuple0)

  /**
    * Method which will take the given RenderableFunction and apply all of the Scalar terms that were parsed,
    * resulting in a RenderableFunction whose one parameter is the Tuple0.
    *
    * @param xs the list of parameters
    * @tparam T the underlying type of the parameters
    * @return a new RenderableFunction (wrapped in Try), whose single input parameter is the Tuple0
    */
  def applyParameters[T](xs: List[Parameter[T]])(implicit tc: ClassTag[T]): Try[RenderableFunction[R]] = {
    @tailrec
    def inner1(r: RenderableFunction[R], work: List[Parameter[T]]): Try[RenderableFunction[R]] =
      work match {
        case Nil => Success(r)
        case h :: t =>
          h match {
            case Left(s) => // T
              // CONSIDER combine common code
              r.partiallyApply(s) match {
                case Success(z) => inner1(z, t)
                case Failure(e) => Failure(e)
              }
            case Right(f) => // Closure[T]
              (for (p <- f(); q <- r.partiallyApply(p)) yield q) match {
                case Success(z) => inner1(z, t)
                case Failure(e) => Failure(e)
              }
          }
      }

    inner1(this, xs)
  }

  def partiallyApply[T](t: T)(implicit tc: ClassTag[T]): Try[RenderableFunction[R]] = RenderableFunction.partiallyApply(arity, func, t, w)

  def partiallyApplyFunction[S, T](f: RenderableFunction[T])(implicit sc: ClassTag[S], tc: ClassTag[T]): Try[RenderableFunction[R]] = RenderableFunction.partiallyApplyFunction[S, T, R](arity, func, w, f)

  /**
    * NOTE that we don't show the function itself because it gives us no additional information
    *
    * @return a String representing this object
    */
  override def toString = s"RenderableFunction($arity, $w)"

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
    FunctionString(f, bind(index, w))
  }
  else throw RenderableFunctionException("FunctionString.partiallyApply cannot apply where arity <= 0")

  def partiallyApplyFunction(w: FunctionString, index: Int = 0): FunctionString = if (arity > index) {
    FunctionString(f, bind(index, w))
  }
  else throw RenderableFunctionException("FunctionString.partiallyApply cannot apply where arity <= 0")

  def arity: Int = {
    def inner(r: Int, _ps: List[Param]): Int = _ps match {
      case Nil => r
      case h :: t =>
          h match {
        case Param(Left(_)) => inner(r, t)
        case Param(Right(Left(_))) => inner(r + 1, t)
        case Param(Right(Right(g))) => inner(r, g.ps ++ t)
      }
    }
    inner(0, ps)
  }

  def bind(index: Int, w: String): List[Param] = bind(Iterator.continually(None).take(index).toList :+ Some(Left(w)))

  def bind(index: Int, w: FunctionString): List[Param] = bind(Iterator.continually(None).take(index).toList :+ Some(Right(w)))

  /**
    * Method to bind some String values to free variables in this FunctionString.
    *
    * @param wos a sequence of optional Strings
    * @return a sequence of Param values where each param is a copy of the corresponding value in ps (the "original"), *unless*
    *         BOTH the original is free AND the optional String is Some(_)
    */
  def bind(wos: List[Option[Either[String,FunctionString]]]): List[Param] = {
    @tailrec def inner(result: List[Param], _ps: List[Param], _ws: List[Option[Either[String,FunctionString]]]): List[Param] = _ps match {
      case Nil => result
      case p :: __ps => p match {
        case Param(Left(_)) => inner(result :+ p, __ps, _ws)
          // TODO merge the code in the following two cases
        case Param(Right(Left(_))) => _ws match {
          case Nil => inner(result :+ p, __ps, _ws)
          case None :: __ws => inner(result :+ p, __ps, __ws)
          case Some(Left(s)) :: __ws => inner(result :+ Param(s"""$s"""), __ps, __ws)
          case Some(Right(g)) :: __ws => inner(result :+ Param(g), __ps, __ws)
        }
      }
    }

    inner(Nil, ps, wos)
  }
}

case class Param(p: Either[String, Either[FreeParam, FunctionString]]) {
  private def isBound = p.isLeft

  private def right = p.right.get

  def isFree: Boolean = !isBound && right.isLeft

  def isFunction: Boolean = !isBound && right.isRight

  override def toString: String = "(" + (p match {
    case Left(s) => s
    case Right(Left(_p)) => _p.toString
    case Right(Right(f)) => f.toString
  }) + ")"
}

case class FreeParam(s: String) {
  override def toString: String = s + FreeParam.query
}

/**
  * Companion object to Closure
  */
object Closure {
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
  def createVarArgsClosure[T](ts: T*)(implicit tc: ClassTag[T]): Closure[T, Seq[T]] = apply(RenderableFunction.varargs(ts.size), ts map (Left(_)):_*)
}

/**
  * Companion object to RenderableFunction
  */
object RenderableFunction {
  // TODO use assert
  def asserting[A](b: => Boolean, w: => String, f: => A): A = if (b) f else throw AssertingError(w)

  def apply[R](arity: Int, func: (Product) => R, w: String)(implicit rc: ClassTag[R]): RenderableFunction[R] = RenderableFunction(arity, func, FunctionString(w, arity))

  def apply[T, R](f: T => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = {
    println(s"apply(T=>R,String): T: $tc, R: $rc")
    asserting(f != null && w != null, "f or w is null",
      apply(1, asTupledFunctionType(f), w)
    )
  }

  def apply[T, R](f: T => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 1))

  def apply[T, R](f: (T, T) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(2, asTupledFunctionType(f), w)
  )

  def apply[T, R](f: (T, T) => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 2))

  def apply[T, R](f: (T, T, T) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(3, asTupledFunctionType(f), w)
  )

  def apply[T, R](f: (T, T, T) => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 3))

  def apply[T, R](f: (T, T, T, T) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(4, asTupledFunctionType(f), w)
  )

  def apply[T, R](f: (T, T, T, T) => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 4))

  def apply[T, R](f: (T, T, T, T, T) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(5, asTupledFunctionType(f), w)
  )

  def apply[T, R](f: (T, T, T, T, T) => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 5))

  def apply[T, R](f: (T, T, T, T, T, T) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(6, asTupledFunctionType(f), w)
  )

  def apply[T, R](f: (T, T, T, T, T, T) => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 6))

  def apply[T, R](f: (T, T, T, T, T, T, T) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(7, asTupledFunctionType(f), w)
  )

  def apply[T, R](f: (T, T, T, T, T, T, T) => R, w: String)(implicit tc: ClassTag[T], rc: ClassTag[R]): RenderableFunction[R] = apply(f, FunctionString(w, 7))

  private def emptyList[T](p: Product) = List[T]()

  private val mkList = "mkList"

  def varargs[T](n: Int)(implicit tc: ClassTag[T]): RenderableFunction[Seq[T]] = n match {
    case 0 => apply(0, emptyList _, mkList)
    case 1 => apply({ t1: T => List[T](t1) }, mkList)
    case 2 => apply({ (t1: T, t2: T) => List[T](t1, t2) }, mkList)
    case 3 => apply({ (t1: T, t2: T, t3: T) => List[T](t1, t2, t3) }, mkList)
    case 4 => apply({ (t1: T, t2: T, t3: T, t4: T) => List[T](t1, t2, t3, t4) }, mkList)
    case 5 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T) => List[T](t1, t2, t3, t4, t5) }, mkList)
    case _ => throw RenderableFunctionException(s"varargs of $n is not implemented")
  }

  def asFunctionType[R](f: Function[_, R]): Product => R = asserting(f != null, "f is null", cast[Product => R](f))

  def asTupledFunctionType[T, R](f: T => R)(implicit tc: ClassTag[T], rc: ClassTag[R]): Product => R = asserting(f != null, "f is null", asFunctionType[R]({ x: Tuple1[T] => f(x._1) }))

  def asTupledFunctionType[T, R](f: (T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def untupled[T, R](f: Tuple1[T] => R): T => R = asserting(f != null, "f is null", { x => f(Tuple1(x)) })

  private def cast[R](x: Any)(implicit rc: ClassTag[R]): R = x match {
    case r: R => r

    case _ => throw new ClassCastException(s"x: $x of type ${x.getClass} cannot be cast to type $rc")
  }

  private def partiallyApply[T, R](n: Int, f: (Product) => R, t: T, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]) = Try(n match {
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
  )

  private def partiallyApplyFunction[S, T, R](n: Int, f: (Product) => R, w: FunctionString, g: RenderableFunction[T])(implicit sc: ClassTag[S], tc: ClassTag[T], rc: ClassTag[R]) = Try(
    g.arity match {
      case 0 => throw RenderableFunctionException("cannot partially apply a RenderableFunction with g having arity 0")
      case 1 =>
        val f1: Tuple1[T] => R = f
        val g1: Tuple1[S] => T = g.func
        val h = RenderableFunction.asFunctionType { x: Tuple1[S] => f1(Tuple1(g1(Tuple1(x._1)))) }
        RenderableFunction(g.arity, h, w.partiallyApplyFunction(g.w))
      case 2 =>
        val f1: Tuple1[T] => R = f
        val g2 = Function.untupled[S, S, T](g.func)
        val h = RenderableFunction.asFunctionType { x: (S, S) => f1(Tuple1(g2.curried(x._1)(x._2))) }
        RenderableFunction(g.arity, h, w.partiallyApplyFunction(g.w))
      // TODO implement up to case 7
      case _ => throw RenderableFunctionException(s"partiallyApplyFunction: functions with arity $n are not supported")
    }
  )

  private def invert[T, R](n: Int, arity: Int, f: (Product) => R, w: FunctionString)(implicit tc: ClassTag[T], rc: ClassTag[R]) = {
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

object Param {
  def apply(s: String): Param = Param(Left(s))

  def apply(p: FreeParam): Param = Param(Right(Left(p)))

  def apply(f: FunctionString): Param = Param(Right(Right(f)))
}

object FreeParam {
  def apply(c: Char): FreeParam = apply(c.toString)

  private val query = """?"""
}

object FunctionString {
  private val params = "abcdefghijklmnopqrstuvwxyz".toList

  def stream(list: List[Char]): Stream[FreeParam] = Stream.cons(FreeParam(list.head), stream(list.tail))

  def apply(f: String, n: Int): FunctionString = FunctionString(f, stream(params) take n map (Param(_)) toList)

  def custom(f: String, cs: List[String]): FunctionString = FunctionString(f, cs map (s => Param(FreeParam(s))))
}

/**
  * RenderableFunctionException
  *
  * @param s the message
  */
case class RenderableFunctionException(s: String) extends Exception(s)

case class AssertingError(w: String) extends Exception(w)

