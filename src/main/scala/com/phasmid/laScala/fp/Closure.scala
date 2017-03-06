/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.Tuple0
import com.phasmid.laScala.{Prefix, Renderable}
import org.slf4j.{Logger, LoggerFactory}

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
  implicit val logger: Logger = Spy.getLogger(getClass)

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
  * A representation of a function call.
  * The function (func) which defines the function call is of form Product=>R where the product (tuple)
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param arity the number of input parameters that are required to fully apply the given function (func)
  * @param func  the function itself
  * @param w     a human-readable representation of the function
  * @param ae    (always evaluate) if true (the default) then this function will be evaluated whenever applyParameters is invoked, otherwise, a new Closure based on this function and the parameter is created.
  * @tparam R the ultimate return type of this RenderableFunction (must support ClassTag)
  */
case class RenderableFunction[R: ClassTag](arity: Int, func: Product => R, w: FunctionString, ae: Boolean = true) extends (Product => Try[R]) with Renderable {

  require(func != null, s"func is null")
  require(w != null, s"w is null")
  require(w.arity == arity, s"arity $arity is not consistent with $w")

  type ApplyParametersResult[T] = (RenderableFunction[R], Seq[Closure[_, T]])

  // NOTE: this line requires R to support ClassTag
  private val rc = implicitly[ClassTag[R]]
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
        Try(func.apply(p)) match {
          case Success(r: Try[R]) => r
          case Failure(x) => Failure(x)
          case _ => Failure(RenderableFunctionException(s"apply($p): logic error"))
        }
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
  def apply[T](t: T): Try[R] = Closure.logDebug(s"$this.apply($t)", apply(Tuple1(t)))

  /**
    * Method to call this RenderableFunction by name only, that's to say without any parameters.
    * If this is a 0-arity function, then Success(r) will be returned. Otherwise, you will get a failure.
    *
    * @return
    */
  def callByName(): Try[R] = Closure.logDebug(s"$this.callByName", apply(Tuple0))

  /**
    * Method which will take the given RenderableFunction and apply all of the given parameters terms,
    * resulting in a RenderableFunction whose one parameter is the Tuple0.
    *
    * NOTE: this method is only used by the Specifications. Probably it should moved there.
    *
    * @param xs the list of parameters
    * @tparam T the underlying type of the parameters
    * @return a new RenderableFunction (wrapped in Try), whose single input parameter is the Tuple0
    */
  def applyAllParameters[T](xs: Seq[Parameter[T]]): Try[RenderableFunction[R]] = Closure.logDebug(s"$this.applyAllParameters",
    applyParameters(xs, evaluateAll = true) match {
      case Success((f, fs)) =>
        if (fs.isEmpty) Success(f)
        else Failure(RenderableFunctionException(s"parameters included at least one call-by-name closure: ${fs.head}"))
      case Failure(x) => Failure(x)
    }
  )

  /**
    * Method which will take the given RenderableFunction and apply the given parameters terms,
    * resulting in an ApplyParametersResult[T]. This type is a tuple of a RenderableFunction[R]
    * and a Seq of Closure[T,R] which represent call-by-name closures that have yet to be applied.
    *
    * @param xs          the list of parameters
    * @param evaluateAll if true then evaluate even call-by-name parameters
    * @tparam T the underlying type of the parameters
    * @return an ApplyParametersResult[T] (wrapped in Try)
    */
  def applyParameters[T](xs: Seq[Parameter[T]], evaluateAll: Boolean): Try[ApplyParametersResult[T]] = {
    implicit val logger = Spy.getLogger(getClass)

    @tailrec
    def inner1(r: ApplyParametersResult[T], work: Seq[Parameter[T]]): ApplyParametersResult[T] =
      work match {
        case Nil => r
        case h :: t =>
          Try(h match {
            case Left(s) =>
              applyParameter(r._1.partiallyApply(s), r._2)
            case Right(f) =>
              if (evaluateAll) applyParameter(for (p <- f.apply(); q <- r._1.partiallyApply(p)) yield q, r._2)
              else if (f.f.ae) (for (p <- f.partiallyApply()) yield p) match {
                case Success(c) =>
                  if (c.arity == 0) applyParameter(r._1.partiallyApply(c()), r._2)
                  else (r._1, r._2 :+ c)
                case Failure(x) => throw x
              }
              else (r._1, r._2 :+ f)
          }
          ) match {
            case Success(rr) => inner1(rr, t)
            case Failure(x) => throw x
          }
      }

    Try(inner1((this, Nil), xs))
  }

  def partiallyApply[T](t: T): Try[RenderableFunction[R]] = RenderableFunction.partiallyApply(arity, func, t, w)

  def partiallyApplyFunction[S, T](f: RenderableFunction[T]): Try[RenderableFunction[R]] = RenderableFunction.partiallyApplyFunction[S, T, R](arity, func, w, f)

  /**
    * NOTE that we don't show the function itself because it gives us no additional information
    *
    * @return a String representing this object
    */
  override def toString = s"RenderableFunction($arity, $evaluationStyle $w)"

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

  private def applyParameter[T](fy: Try[RenderableFunction[R]], ps: Seq[Closure[_, T]]) = fy match {
    case Success(f) => (f, ps)
    case Failure(e) => throw e
  }

  private def evaluationStyle = if (ae) "" else "=>"
}

case class FunctionString(f: String, ps: Seq[Param]) {
  def invert(n: Int): FunctionString = {
    @tailrec def inner(r1: List[Param], r2: List[Param], i: Int, _ps: List[Param]): Seq[Param] = _ps match {
      case Nil => r1 ++ r2
      case h :: t => if (i < n) inner(h +: r1, r2, i + 1, t)
      else inner(r1, r2 :+ h, i + 1, t)
    }

    FunctionString(f, inner(Nil, Nil, 0, ps.toList))
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
          case Param(Right(Right(g))) => inner(r, g.ps.toList ++ t)
        }
    }

    inner(0, ps.toList)
  }

  def bind(index: Int, w: String): Seq[Param] = bind(Iterator.continually(None).take(index).toList :+ Some(Left(w)))

  def bind(index: Int, w: FunctionString): Seq[Param] = bind(Iterator.continually(None).take(index).toList :+ Some(Right(w)))

  /**
    * Method to bind some String values to free variables in this FunctionString.
    *
    * @param wos a sequence of optional Strings
    * @return a sequence of Param values where each param is a copy of the corresponding value in ps (the "original"), *unless*
    *         BOTH the original is free AND the optional String is Some(_)
    */
  def bind(wos: Seq[Option[Either[String, FunctionString]]]): Seq[Param] = {
    @tailrec def inner(result: List[Param], _ps: List[Param], _ws: List[Option[Either[String, FunctionString]]]): Seq[Param] = _ps match {
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

    inner(Nil, ps.toList, wos.toList)
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

/**
  * Companion object to RenderableFunction
  */
object RenderableFunction {

  // TODO use assert
  def asserting[A](b: => Boolean, w: => String, f: => A): A = if (b) f else throw AssertingError(w)

  def apply[R: ClassTag](arity: Int, func: (Product) => R, w: String, ae: Boolean): RenderableFunction[R] = RenderableFunction(arity, func, FunctionString(w, arity), ae)

  /**
    * The following apply functions are in pairs: one with FunctionString and one with just String as the second parameter.
    *
    * @param f a function of some mix of T resulting in R
    * @param w a String or a FunctionString
    * @tparam T the parameter type
    * @tparam R the result type
    * @return a new RenderableFunction
    */
  def apply[T, R: ClassTag](f: T => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = {
    asserting(f != null && w != null, "f or w is null",
      apply(1, asTupledFunctionType(f), w, ae)
    )
  }

  def apply[T, R: ClassTag](f: T => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 1), ae)

  def apply[T1, T2, R: ClassTag](f: (T1, T2) => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(2, asTupledFunctionType(f), w, ae)
  )

  def apply[T1, T2, R: ClassTag](f: (T1, T2) => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 2), ae)

  def apply[T1, T2, T3, R: ClassTag](f: (T1, T2, T3) => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(3, asTupledFunctionType(f), w, ae)
  )

  def apply[T1, T2, T3, R: ClassTag](f: (T1, T2, T3) => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 3), ae)

  def apply[T1, T2, T3, T4, R: ClassTag](f: (T1, T2, T3, T4) => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(4, asTupledFunctionType(f), w, ae)
  )

  def apply[T1, T2, T3, T4, R: ClassTag](f: (T1, T2, T3, T4) => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 4), ae)

  def apply[T1, T2, T3, T4, T5, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(5, asTupledFunctionType(f), w, ae)
  )

  def apply[T1, T2, T3, T4, T5, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 5), ae)

  def apply[T1, T2, T3, T4, T5, T6, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(6, asTupledFunctionType(f), w, ae)
  )

  def apply[T1, T2, T3, T4, T5, T6, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 6), ae)

  def apply[T1, T2, T3, T4, T5, T6, T7, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: FunctionString, ae: Boolean): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(7, asTupledFunctionType(f), w, ae)
  )

  def apply[T1, T2, T3, T4, T5, T6, T7, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: String, ae: Boolean): RenderableFunction[R] = apply(f, FunctionString(w, 7), ae)

  private def emptyList[T](p: Product) = Seq[T]()

  private val mkList = "mkList"

  def varargs[T](n: Int): RenderableFunction[Seq[T]] = n match {
    case 0 => apply(0, emptyList _, mkList, ae = true)
    case 1 => apply({ t1: T => Seq[T](t1) }, mkList, true)
    case 2 => apply({ (t1: T, t2: T) => Seq[T](t1, t2) }, mkList, true)
    case 3 => apply({ (t1: T, t2: T, t3: T) => Seq[T](t1, t2, t3) }, mkList, true)
    case 4 => apply({ (t1: T, t2: T, t3: T, t4: T) => Seq[T](t1, t2, t3, t4) }, mkList, true)
    case 5 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T) => Seq[T](t1, t2, t3, t4, t5) }, mkList, true)
    case _ => throw RenderableFunctionException(s"varargs of $n is not implemented")
  }

  def asFunctionType[R](f: Function[_, R]): Product => R = asserting(f != null, "f is null", cast[Product => R](f))

  def asTupledFunctionType[T, R](f: T => R): Product => R = asserting(f != null, "f is null", asFunctionType[R]({ x: Tuple1[T] => f(x._1) }))

  def asTupledFunctionType[T, R](f: (T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T, R](f: (T, T, T, T, T, T, T, T) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def untupled[T, R](f: Tuple1[T] => R): T => R = asserting(f != null, "f is null", { x => f(Tuple1(x)) })

  // NOTE: This method requires R to support ClassTag
  private def cast[R: ClassTag](x: Any): R = x match {
    case r: R => r
    case _ => throw new ClassCastException(s"x: $x of type ${x.getClass} cannot be cast to type ${implicitly[ClassTag[R]]}")
  }

  private def partiallyApply[T, R: ClassTag](n: Int, f: (Product) => R, t: T, w: FunctionString) = Try(n match {
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

  private def partiallyApplyFunction[S, T, R: ClassTag](n: Int, f: (Product) => R, w: FunctionString, g: RenderableFunction[T]) = Try(
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

  private def invert[T, R: ClassTag](n: Int, arity: Int, f: (Product) => R, w: FunctionString) = {
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

  def stream(xs: Seq[Char]): Stream[FreeParam] = Stream.cons(FreeParam(xs.head), stream(xs.tail))

  def apply(f: String, n: Int): FunctionString = FunctionString(f, stream(params) take n map (Param(_)) toList)

  def custom(f: String, cs: Seq[String]): FunctionString = FunctionString(f, cs map (s => Param(FreeParam(s))))
}

/**
  * RenderableFunctionException
  *
  * @param s the message
  */
case class RenderableFunctionException(s: String) extends Exception(s)

case class AssertingError(w: String) extends Exception(w)

