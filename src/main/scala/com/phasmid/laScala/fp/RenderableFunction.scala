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
  * A representation of a function call.
  * The function (func) which defines the function call is of form Product=>R where the product (tuple)
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param arity the number of input parameters that are required to fully apply the given function (func)
  * @param func  the function itself
  * @param w     a human-readable representation of the function
  * @param cbn   a sequence of booleans, corresponding to each of the parameters of (untupled) func. If the boolean is true, then that parameter is call-by-name, i.e. a Function0[T] rather than a T.
  * @tparam R the ultimate return type of this RenderableFunction (must support ClassTag)
  */
case class RenderableFunction[R: ClassTag](arity: Int, func: (Product) => R, w: FunctionString, cbn: Seq[Boolean]) extends (Product => Try[R]) with Renderable {

  implicit val logger = Spy.getLogger(getClass)

  require(func != null, s"func is null")
  require(w != null, s"w is null")
  require(w.arity == arity, s"arity $arity is not consistent with $w")
  require(cbn.size == arity, s"cbn sequence: has size ${cbn.size} which is not equal to $arity")

  type ApplyParametersResult[T] = (RenderableFunction[R], Seq[Closure[_, T]])

  // NOTE: this line requires R to support ClassTag
  private val rc = implicitly[ClassTag[R]]
  private val returnIsTry = rc.runtimeClass == classOf[Try[_]]

  println(s"created: RenderableFunction with arity=$arity, func=$w; cbn=$cbn; rc=$rc; returnIsTry=$returnIsTry; alwaysEvaluate=$alwaysEvaluate ")

  /**
    * Apply parameter p to this function.
    *
    * NOTE that if R is Try[S], then the result of apply will be R, not Try[R]
    *
    * @param p a Tuple
    * @return an R wrapped in Try
    */
  def apply(p: Product): Try[R] =
    if (arity == p.productArity) {
      val ry = Spy.spy(s"$this.apply($p): ry",Try(func(p)))
      println(s"RenderableFunction.apply($p): ry=$ry, returnIsTry=$returnIsTry")
      if (returnIsTry)
        ry match {
          case Success(r: Try[R]) => r
          case Failure(x) => Failure(x)
          case _ => Failure(RenderableFunctionException(s"apply($p): logic error"))
        }
      else
        ry
    }
    else
      Failure(RenderableFunctionException(s"cannot apply Product of arity ${p.productArity}: $p to RenderableFunction: ($this)"))

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
    * NOTE: this method is only used by the Specifications. Probably it should be moved there.
    * No, that is not true.
    *
    * @param xs the list of parameters
    * @tparam T the underlying type of the parameters
    * @return a new RenderableFunction (wrapped in Try), whose single input parameter is the Tuple0
    */
  @deprecated
  def applyAllParameters[T](xs: Seq[Parameter[T]]): Try[RenderableFunction[R]] = Closure.logDebug(s"$this.applyAllParameters",
    // CHECK -- I put this to false. Is that OK?
    applyParameters(xs, evaluateAll = false)
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
  def applyParameters[T](xs: Seq[Parameter[T]], evaluateAll: Boolean): Try[RenderableFunction[R]] = {

    println(s"applyParameters($xs, $evaluateAll) to $this")

    def partiallApply(rfy: Try[RenderableFunction[R]], tp: Parameter[T]): Try[RenderableFunction[R]] = tp match {
      case Left(t) =>
        Spy.spy(s"left branch: $t: result", rfy flatMap (_.partiallyApply(t)))
      case Right(c) => // c: Closure[_,T]
        val gy: Try[Closure[_, T]] = Spy.spy(s"right branch: $c: gy", c.partiallyApply())
        Spy.spy(s"right branch: $c: result", for (g <- gy; if g.ps.isEmpty; f1 = Spy.spy("f1", g.f); rf <- rfy; f2 <- Spy.spy("f2", rf.partiallyApplyX({ () => f1.callByName.get }, f1.w))) yield f2)
    }

    @tailrec
    def inner1(rfy: Try[RenderableFunction[R]], work: Seq[Parameter[T]]): Try[RenderableFunction[R]] =
      work match {
        case Nil => rfy
        case tp :: tps => inner1(partiallApply(rfy, tp),tps)
      }

    inner1(Try(this), xs)
  }

//  /**
//    * Method which will take the given RenderableFunction and apply the given parameters terms,
//    * resulting in an ApplyParametersResult[T]. This type is a tuple of a RenderableFunction[R]
//    * and a Seq of Closure[T,R] which represent call-by-name closures that have yet to be applied.
//    *
//    * @param xs          the list of parameters
//    * @param evaluateAll if true then evaluate even call-by-name parameters
//    * @tparam T the underlying type of the parameters
//    * @return an ApplyParametersResult[T] (wrapped in Try)
//    */
//  def applyParameters[T](xs: Seq[Parameter[T]], evaluateAll: Boolean): Try[ApplyParametersResult[T]] = {
//
//    println(s"applyParameters($xs, $evaluateAll) to $this")
//
//    @tailrec
//    def inner1(r: ApplyParametersResult[T], work: Seq[Parameter[T]]): ApplyParametersResult[T] =
//      work match {
//        case Nil => r
//        case h :: t =>
//          Try(h match {
//            case Left(s) =>
//              applyParameter(r._1.partiallyApply(s), r._2)
//            case Right(f) => // f: Closure[_,T]
//              (r._1, r._2 :+ f.partiallyApply().get)
////              // TODO eliminate the possibilities of evaluateAll and f.f.alwaysEvaluate
////              println(s"inner1(1): evaluateAll: $evaluateAll, f.f.alwaysEvaluate: ${f.f.alwaysEvaluate}, f=$f")
////              if (evaluateAll) applyParameter(for (p <- f(); q <- r._1.partiallyApply(p)) yield q, r._2)
////              else if (f.f.alwaysEvaluate) (for (p <- f.partiallyApply()) yield p) match {
////                case Success(c) =>
////                  println(s"inner1(2): c: $c")
//////                  if (c.arity == 0) applyParameter(r._1.partiallyApply(c()), r._2)
//////                  else
////              (r._1, r._2 :+ c)
////                case Failure(x) => throw x
////              }
////              else {
////                println(s"inner1(3): r=$r, f=$f")
////                // TODO avoid having to use get here
////                (r._1, r._2 :+ f.partiallyApply(false).get)
////              }
//          }
//          ) match {
//            case Success(rr) => inner1(rr, t)
//            case Failure(x) => throw x
//          }
//      }
//
//    Try(inner1((this, Nil), xs))
//  }

  /**
    * Method to partially apply this RenderableFunction, given the provided list of parameters, and resulting in a Closure
    *
    * @param xs a varargs sequence of Parameter[T]
    * @return a Closure[R] wrapped in Try.
    */
  def partiallyApply[T](xs: Parameter[T]*): Try[Closure[T, R]] = for (g: RenderableFunction[R] <- applyParameters[T](xs.toList, false); z = xs map (Right(_))) yield Closure[T, R](g)

  def partiallyApply[T](t: T): Try[RenderableFunction[R]] = RenderableFunction.partiallyApply(arity, func, t, w, cbn)

  // TODO change name here and that of the called method
  def partiallyApplyX[T](f: () => T, z: FunctionString): Try[RenderableFunction[R]] = RenderableFunction.partiallyApplyX(arity, func, f, w.partiallyApplyFunction(z), cbn)

  def partiallyApplyFunction[S, T](f: RenderableFunction[T]): Try[RenderableFunction[R]] = RenderableFunction.partiallyApplyFunction[S, T, R](arity, func, w, f, cbn)

  /**
    * NOTE that we don't show the function itself because it gives us little additional information (only the actual arity of the function)
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
  def invert[T](n: Int): RenderableFunction[R] = RenderableFunction.invert(n, arity, func, w, cbn)

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
  def render(indent: Int)(implicit tab: (Int) => Prefix): String = w.toString

  private def applyParameter[T](fy: Try[RenderableFunction[R]], ps: Seq[Closure[_, T]]) = fy match {
    case Success(f) => (f, ps)
    case Failure(e) => throw e
  }

  /**
    *
    * @return if true (the default) then this function will be evaluated whenever applyParameters is invoked, otherwise, a new Closure based on this function and the parameter is created.
    */
  private def alwaysEvaluate = cbn.forall(!_)

  private def evaluationStyle = if (alwaysEvaluate) "" else "=>"
}

/**
  * Class to represent a (renderable) function string.
  * // TODO we ought to show parameters that are call-by-name
  *
  * @param f  the name of the function (not necessarily the same as the original definition of the corresponding function/method)
  * @param ps a sequence of Param objects (these represent bound or unbound parameters)
  */
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
        case Param(Right(Left(_))) => _ws match {
          case Nil => inner(result :+ p, __ps, _ws)
          case None :: __ws => inner(result :+ p, __ps, __ws)
          case Some(Left(s)) :: __ws => inner(result :+ Param(s"""$s"""), __ps, __ws)
          case Some(Right(g)) :: __ws => inner(result :+ Param(g), __ps, __ws)
          case _ => throw RenderableFunctionException(s"no match for work list ${_ws}")
        }
          // FIXME create a case for (concat(lookup("c1")))
        case _ => throw RenderableFunctionException(s"no match for parameter p: $p")
      }
      case _ => throw RenderableFunctionException(s"no match for parameters ${_ps}")
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

case class FreeParam(s: String, cbn: Boolean) {
  override def toString: String = FP.which(cbn)("=>", "") + s + FreeParam.query
}

/**
  * Companion object to RenderableFunction
  */
object RenderableFunction {

  // TODO use assert
  def asserting[A](b: => Boolean, w: => String, f: => A): A = if (b) f else throw AssertingError(w)

  def apply[R: ClassTag](arity: Int, func: (Product) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = RenderableFunction(arity, func, FunctionString(w, arity, cbn), cbn)

  /**
    * The following apply functions are in pairs: one with FunctionString and one with just String as the second parameter.
    *
    * @param f a function of some mix of T resulting in R
    * @param w a String or a FunctionString
    * @tparam R the result type
    * @return a new RenderableFunction
    */
  def apply[R: ClassTag](f: () => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = {
    println(s"apply: w=$w; cbn=$cbn")
    asserting(f != null && w != null, "f or w is null",
      apply(0, asTupledFunctionType(f), w, cbn)
    )
  }

  def apply[R: ClassTag](f: () => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 1, cbn), cbn)

  def apply[T, R: ClassTag](f: T => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = {
    println(s"apply: w=$w; cbn=$cbn")
    asserting(f != null && w != null, "f or w is null",
      apply(1, asTupledFunctionType(f), w, cbn)
    )
  }

  def apply[T, R: ClassTag](f: T => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 1, cbn), cbn)

  def apply[T1, T2, R: ClassTag](f: (T1, T2) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(2, asTupledFunctionType(f), w, cbn)
  )

  def apply[T1, T2, R: ClassTag](f: (T1, T2) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 2, cbn), cbn)

  def apply[T1, T2, T3, R: ClassTag](f: (T1, T2, T3) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(3, asTupledFunctionType(f), w, cbn)
  )

  def apply[T1, T2, T3, R: ClassTag](f: (T1, T2, T3) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 3, cbn), cbn)

  def apply[T1, T2, T3, T4, R: ClassTag](f: (T1, T2, T3, T4) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(4, asTupledFunctionType(f), w, cbn)
  )

  def apply[T1, T2, T3, T4, R: ClassTag](f: (T1, T2, T3, T4) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 4, cbn), cbn)

  def apply[T1, T2, T3, T4, T5, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(5, asTupledFunctionType(f), w, cbn)
  )

  def apply[T1, T2, T3, T4, T5, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 5, cbn), cbn)

  def apply[T1, T2, T3, T4, T5, T6, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(6, asTupledFunctionType(f), w, cbn)
  )

  def apply[T1, T2, T3, T4, T5, T6, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 6, cbn), cbn)

  def apply[T1, T2, T3, T4, T5, T6, T7, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(7, asTupledFunctionType(f), w, cbn)
  )

  def apply[T1, T2, T3, T4, T5, T6, T7, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 7, cbn), cbn)

  private def emptyList[T](p: Product) = Seq[T]()

  private val mkList = "mkList"

  def callByName(n: Int): Seq[Boolean] = Stream.continually(true) take n

  def callByValue(n: Int): Seq[Boolean] = Stream.continually(false) take n

  def varargs[T](n: Int): RenderableFunction[Seq[T]] = n match {
    case 0 => apply(0, emptyList _, mkList, callByValue(n))
    case 1 => apply({ t1: T => Seq[T](t1) }, mkList, callByValue(n))
    case 2 => apply({ (t1: T, t2: T) => Seq[T](t1, t2) }, mkList, callByValue(n))
    case 3 => apply({ (t1: T, t2: T, t3: T) => Seq[T](t1, t2, t3) }, mkList, callByValue(n))
    case 4 => apply({ (t1: T, t2: T, t3: T, t4: T) => Seq[T](t1, t2, t3, t4) }, mkList, callByValue(n))
    case 5 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T) => Seq[T](t1, t2, t3, t4, t5) }, mkList, callByValue(n))
    case _ => throw RenderableFunctionException(s"varargs of $n is not implemented")
  }

  def asFunctionType[R](f: Function[_, R]): Product => R = asserting(f != null, "f is null", cast[Product => R](f))

  def asTupledFunctionType[R](f: () => R): Product => R = {println(s"asTupledFunctionType f=$f"); asserting(f != null, "f is null", asFunctionType[R]({ x: Product => f() }))}

  def asTupledFunctionType[T1, R](f: T1 => R): Product => R = {println(s"asTupledFunctionType f=$f"); asserting(f != null, "f is null", asFunctionType[R]({ x: Tuple1[T1] => f(x._1) }))}

  def asTupledFunctionType[T1, T2, R](f: (T1, T2) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T1, T2, T3, R](f: (T1, T2, T3) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def asTupledFunctionType[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def untupled[T, R](f: Tuple1[T] => R): T => R = asserting(f != null, "f is null", { x => f(Tuple1(x)) })

  // NOTE: This method requires R to support ClassTag
  private def cast[R: ClassTag](x: Any): R = x match {
    case r: R => r
    case _ => throw new ClassCastException(s"x: $x of type ${x.getClass} cannot be cast to type ${implicitly[ClassTag[R]]}")
  }

  private def getPartialApplicationFunction[T, R: ClassTag](n: Int, f: (Product) => R, t: T, w: FunctionString, cbn: Boolean): (Product) => R =
    if (cbn) getPartialApplicationFunction(n, f, { () => t }, w, false)
    else
      n match {
        case 0 => throw RenderableFunctionException("cannot partially apply a RenderableFunction of arity 0")
        case 1 =>
          val f1: Tuple1[T] => R = f
          val g0: (Product) => R = { _: Product => f1(Tuple1(t)) }
          g0
        case 2 =>
          val f2 = Function.untupled[T, Any, R](f)
          val g1 = RenderableFunction.asFunctionType { x: Tuple1[_] => f2.curried(t)(x._1) }
          g1
        case 3 =>
          val f3 = Function.untupled[T, Any, Any, R](f)
          val g2 = RenderableFunction.asFunctionType { x: (_, _) => f3.curried(t)(x._1)(x._2) }
          g2
        case 4 =>
          val f4 = Function.untupled[T, Any, Any, Any, R](f)
          val g3 = RenderableFunction.asFunctionType { x: (_, _, _) => f4.curried(t)(x._1)(x._2)(x._3) }
          g3
        case 5 =>
          val f5 = Function.untupled[T, T, T, T, T, R](f)
          val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f5.curried(t)(x._1)(x._2)(x._3)(x._4) }
          g4
        case 6 =>
          val f6 = FP.untupled[T, T, T, T, T, T, R](f)
          val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f6.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5) }
          g5
        case 7 =>
          val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
          val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
          g6
        case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
      }

  private def getPartialApplicationFunctionX[T, R: ClassTag](n: Int, f: (Product) => R, t: () => T, w: FunctionString, cbn: Boolean): (Product) => R =
      n match {
        case 0 => throw RenderableFunctionException("cannot partially apply a RenderableFunction of arity 0")
        case 1 =>
          val f1: Tuple1[T] => R = f
          val g0: (Product) => R = { _: Product => f1(Tuple1(t())) }
          g0
        case 2 =>
          val f2 = Function.untupled[T, Any, R](f)
          val g1 = RenderableFunction.asFunctionType { x: Tuple1[_] => f2.curried(t())(x._1) }
          g1
        case 3 =>
          val f3 = Function.untupled[T, Any, Any, R](f)
          val g2 = RenderableFunction.asFunctionType { x: (_, _) => f3.curried(t())(x._1)(x._2) }
          g2
        case 4 =>
          val f4 = Function.untupled[T, Any, Any, Any, R](f)
          val g3 = RenderableFunction.asFunctionType { x: (_, _, _) => f4.curried(t())(x._1)(x._2)(x._3) }
          g3
        case 5 =>
          val f5 = Function.untupled[T, T, T, T, T, R](f)
          val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f5.curried(t())(x._1)(x._2)(x._3)(x._4) }
          g4
        case 6 =>
          val f6 = FP.untupled[T, T, T, T, T, T, R](f)
          val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f6.curried(t())(x._1)(x._2)(x._3)(x._4)(x._5) }
          g5
        case 7 =>
          val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
          val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t())(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
          g6
        case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
      }

  private def partiallyApply[T, R: ClassTag](n: Int, f: (Product) => R, t: T, w: FunctionString, cbn: Seq[Boolean]): Try[RenderableFunction[R]] = Try {
    val g0 = getPartialApplicationFunction(n, f, t, w, cbn.head)
    val s = w.partiallyApply(t)
    RenderableFunction(n - 1, g0, s, cbn.tail)
  }

  private def partiallyApplyX[T, R: ClassTag](n: Int, f: (Product) => R, tf: () => T, w: FunctionString, cbn: Seq[Boolean]): Try[RenderableFunction[R]] = Try {
    val g0 = getPartialApplicationFunctionX(n, f, tf, w, cbn.head)
    RenderableFunction(n - 1, g0, w, cbn.tail)
  }

  private def partiallyApplyFunction[S, T, R: ClassTag](n: Int, f: (Product) => R, w: FunctionString, g: RenderableFunction[T], cbn: Seq[Boolean]) = Try {
    // TODO do something with this!
    val x = cbn.head
    g.arity match {
      case 0 => throw RenderableFunctionException("cannot partially apply a RenderableFunction with g having arity 0")
      case 1 =>
        val f1: Tuple1[T] => R = f
        val g1: Tuple1[S] => T = g.func
        val h = RenderableFunction.asFunctionType { x: Tuple1[S] => f1(Tuple1(g1(Tuple1(x._1)))) }
        RenderableFunction(g.arity, h, w.partiallyApplyFunction(g.w), cbn.tail)
      case 2 =>
        val f1: Tuple1[T] => R = f
        val g2 = Function.untupled[S, S, T](g.func)
        val h = RenderableFunction.asFunctionType { x: (S, S) => f1(Tuple1(g2.curried(x._1)(x._2))) }
        RenderableFunction(g.arity, h, w.partiallyApplyFunction(g.w), cbn.tail)
      // TODO implement up to case 7
      case _ => throw RenderableFunctionException(s"partiallyApplyFunction: functions with arity $n are not supported")
    }
  }

  private def invert[T, R: ClassTag](n: Int, arity: Int, f: (Product) => R, w: FunctionString, cbn: Seq[Boolean]) = {
    if (arity < 2 || n > arity)
      throw RenderableFunctionException(s"can't invert $n parameters of a RenderableFunction of arity $arity")
    else {
      val cbni = ((cbn take (cbn.size - n)) :+ cbn(n - 1) :+ cbn(n - 2)) ++ (cbn drop n)
      arity match {
        case 2 =>
          val f2 = FP.untupled[T, T, R](f).curried
          val f2i = n match {
            case 2 => FP.invert2(f2)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g2 = RenderableFunction.asFunctionType { x: (T, T) => f2i(x._1)(x._2) }
          RenderableFunction(n, g2, w.invert(n), cbni)
        case 3 =>
          val f3 = FP.untupled[T, T, T, R](f).curried
          val f3i = n match {
            case 2 => FP.invert2(f3)
            case 3 => FP.invert3(f3)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g3 = RenderableFunction.asFunctionType { x: (T, T, T) => f3i(x._1)(x._2)(x._3) }
          RenderableFunction(n, g3, w.invert(n), cbni)
        case 4 =>
          val f4 = FP.untupled[T, T, T, T, R](f).curried
          val f4i = n match {
            case 2 => FP.invert2(f4)
            case 3 => FP.invert3(f4)
            case 4 => FP.invert4(f4)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f4i(x._1)(x._2)(x._3)(x._4) }
          RenderableFunction(n, g4, w.invert(n), cbni)
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
          RenderableFunction(n, g5, w.invert(n), cbni)
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
          RenderableFunction(n, g6, w.invert(n), cbni)
        case _ => throw RenderableFunctionException(s"invert with arity $arity is not supported")
      }
    }
  }
}

object Param {
  def apply(s: String): Param = Param(Left(s))

  def apply(p: FreeParam): Param = Param(Right(Left(p)))

  def apply(f: FunctionString): Param = Param(Right(Right(f)))
}

object FreeParam {
  def apply(x: (Char, Boolean)): FreeParam = apply(x._1.toString, x._2)

  def apply(c: Char): FreeParam = apply(c.toString, cbn = false)

  private val query = """?"""
}

object FunctionString {
  private val params = "abcdefghijklmnopqrstuvwxyz".toList

  private def stream(xs: Seq[(Char, Boolean)]): Stream[FreeParam] = xs match {
    case h :: t => Stream.cons(FreeParam(h), stream(t))
    case Nil => Stream.empty
  }

  def apply(f: String, n: Int, cbn: Seq[Boolean] = Stream.continually(false)): FunctionString = FunctionString(f, stream(params zip cbn) take n map (Param(_)) toList)

  def custom(f: String, cs: Seq[String], cbn: Seq[Boolean] = Stream.continually(false)): FunctionString = FunctionString(f, (cs zip cbn) map { case (s, b) => Param(FreeParam(s, b)) })
}

/**
  * RenderableFunctionException
  *
  * @param s the message
  */
case class RenderableFunctionException(s: String) extends Exception(s)

case class AssertingError(w: String) extends Exception(w)

