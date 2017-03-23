/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.values.Tuple0
import com.phasmid.laScala.{Prefix, Renderable}

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.reflect.ClassTag
import scala.util._

/**
  * A representation of a function call.
  * The function (func) which defines the function call is of form Product=>R where the product (tuple)
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param arity the number of input parameters that are required to fully apply the given function (func)
  * @param w     a human-readable representation of the function
  * @param cbn   a sequence of booleans, corresponding to each of the parameters of (untupled) func. If the boolean is true, then that parameter is call-by-name, i.e. a Function0[T] rather than a T.
  * @param f     the function itself (this field is not considered in the hashCode and equals methods)
  * @tparam R the ultimate return type of this RenderableFunction (must support ClassTag)
  */
case class RenderableFunction[R: ClassTag](arity: Int, w: FunctionString, cbn: Seq[Boolean])(f: (Product) => R) extends (Product => Try[R]) with Renderable {

  implicit private val logger = Spy.getLogger(getClass)

  require(f != null, s"f is null")
  require(w != null, s"w is null")
  require(w.arity == arity, s"arity $arity is not consistent with $w")
  require(cbn.size == arity, s"cbn sequence: has size ${cbn.size} which is not equal to $arity")

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
    if (arity == p.productArity) {
      val ry = Spy.spy(s"$this.apply($p): ry",Try(func(p)))
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
    * Method to get the function that is the basis of this RenderableFunction
    *
    * @return the function as a Product=>R
    */
  def func: (Product) => R = f

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
  def applyParameters[T](xs: Seq[Parameter[T]], evaluateAll: Boolean = false): Try[RenderableFunction[R]] = {
    def partiallApply(rfy: Try[RenderableFunction[R]], tp: Parameter[T]): Try[RenderableFunction[R]] = tp match {
      case Left(t) =>
        Spy.spy(s"left branch: $t: result", rfy flatMap (_.partiallyApply(t)))
      case Right(c) => // c: Closure[_,T]
        val gy: Try[Closure[_, T]] = Spy.spy(s"right branch: $c: gy", c.partiallyApply())
        Spy.spy(s"right branch: $c: result", for (g <- gy; if g.ps.isEmpty; f1 = Spy.spy("f1", g.f); rf <- rfy; f2 <- Spy.spy("f2", rf.partiallyApplyFunction({ () => f1.callByName().get }, f1.w))) yield f2)
    }

    @tailrec
    def inner1(rfy: Try[RenderableFunction[R]], work: Seq[Parameter[T]]): Try[RenderableFunction[R]] =
      work match {
        case Nil => rfy
        case tp :: tps => inner1(partiallApply(rfy, tp), tps)
      }

    inner1(Try(this), xs)
  }

  /**
    * Method to partially apply this RenderableFunction, given the provided list of parameters, and resulting in a Closure
    *
    * @param xs a varargs sequence of Parameter[T]
    * @return a Closure[R] wrapped in Try.
    */
  def partiallyApply[T](xs: Parameter[T]*): Try[Closure[T, R]] = for (g: RenderableFunction[R] <- applyParameters[T](xs.toList)) yield Closure[T, R](g)

  /**
    * Method to partially apply the value t to this RenderableFunction
    *
    * @param t a T value
    * @tparam T
    * @return
    */
  def partiallyApply[T](t: T): Try[RenderableFunction[R]] = RenderableFunction.partiallyApply(arity, func, t, w.partiallyApply(t), cbn)

  /**
    * Method to partially apply the function f to this RenderableFunction
    *
    * @param f
    * @param z
    * @tparam T
    * @return
    */
  def partiallyApplyFunction[T](f: () => T, z: FunctionString): Try[RenderableFunction[R]] = RenderableFunction.partiallyApplyFunction(arity - 1, func, f, w.partiallyApplyFunction(z), cbn)

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

  /**
    *
    * @return if true (the default) then this function will be evaluated whenever applyParameters is invoked, otherwise, a new Closure based on this function and the parameter is created.
    */
  private def alwaysEvaluate = cbn.forall(!_)

  private def evaluationStyle = if (alwaysEvaluate) "" else "=>"
}

/**
  * Class to represent a (renderable) function string.
  *
  * @param f  the name of the function (not necessarily the same as the original definition of the corresponding function/method)
  * @param ps a sequence of Param objects (these represent bound or unbound parameters)
  */
case class FunctionString(f: String, ps: Seq[Param]) {
  implicit private val logger = Spy.getLogger(getClass)

  implicit def convertBooleanToInt(b: Boolean): Int = if (b) 1 else 0

  def invert(n: Int): FunctionString = {
    @tailrec def inner(r1: List[Param], r2: List[Param], i: Int, _ps: List[Param]): Seq[Param] = _ps match {
      case Nil => r1 ++ r2
      case h :: t => if (i < n) inner(h +: r1, r2, i + 1, t)
      else inner(r1, r2 :+ h, i + 1, t)
    }

    FunctionString(f, inner(Nil, Nil, 0, ps.toList))
  }

  override def toString: String = f + ps.mkString("", "", "")

  def partiallyApply[T](t: => T, skip: Int = 0): FunctionString =
    if (skip < ps.length) {
      val quotedStringR = """\"([^\"]*)\"""".r
    val w = java.util.regex.Matcher.quoteReplacement(t match {
      case s: String => s match {
        case quotedStringR(_) => s
        case _ => s""""$t""""
      }
      case _ => s"$t"
    })
      Spy.spy(s"partiallyApply($t,$skip)", FunctionString(f, bind(bound + skip, w)))
    }
    else throw RenderableFunctionException(s"FunctionString.partiallyApply cannot apply $t at $skip to $this")

  def partiallyApplyFunction(w: FunctionString, skip: Int = 0): FunctionString =
    if (skip < ps.length) FunctionString(f, bind(bound + skip, w))
    else throw RenderableFunctionException(s"FunctionString.partiallyApply cannot apply $w at $skip to $this where arity <= $skip")

  /**
    * @return the arity of this FunctionString
    */
  def arity: Int = {
    def inner(r: Int, w: Seq[Param]): Int = w match {
      case Nil => r
      case h :: t =>
        h match {
          case Param(Left(_)) => inner(r, t)
          case Param(Right(Left(_))) => inner(r + 1, t)
          case Param(Right(Right(g))) => inner(r, g.ps ++ t)
        }
    }

    Spy.spy(s"arity of $this", inner(0, ps))
  }

  /**
    * @return the number of bound variables which begin the list of Params. As soon as an unbound Param is found, we return the current count.
    */
  def bound: Int = {
    import scala.language.implicitConversions
    def inner(r: Int, w: Seq[Param]): Int = w match {
      case Nil => r
      case h :: t => inner(r + h.isBound, t)
    }

    Spy.spy(s"bound of $this", inner(0, ps))
  }

  def bind(index: Int, w: String): Seq[Param] = Spy.spy(s"$this bind($index,$w)", bind(FunctionString.nones.take(index).toSeq :+ Some(Left(w))))

  def bind(index: Int, w: FunctionString): Seq[Param] = Spy.spy(s"$this bind($index,$w)", bind(FunctionString.nones.take(index).toSeq :+ Some(Right(w))))

  /**
    * Method to bind some String values to free variables in this FunctionString.
    *
    * @param wos a sequence of optional Strings
    * @return a sequence of Param values where each param is a copy of the corresponding value in ps (the "original"), *unless*
    *         BOTH the original is free AND the optional String is Some(_)
    */
  def bind(wos: Seq[Option[Either[String, FunctionString]]]): Seq[Param] = {
    require(wos.length <= ps.length, s"wos is too long (${wos.length}) for this FunctionString: $this")

    @tailrec def inner(result: Seq[Param], work: Seq[(Param, Option[Either[String, FunctionString]])]): Seq[Param] = {
      Spy.spy(s"bind.inner($result, $work", ())
      work match {
        case Nil => result
        case h :: t =>
          val p1 = h._2 match {
            case Some(Left(s)) => h._1 bind s
            case Some(Right(g)) => h._1 bind g
            case None => h._1
          }
          inner(result :+ p1, t)
      }
    }

    Spy.spy("result of bind", inner(Nil, ps zipAll(wos, null, None)))
  }

}

case class Param(p: Either[String, Either[FreeParam, FunctionString]]) {

  def bind(w: String): Param = p match {
    case Left(_) => this // already bound
    case Right(Left(_)) => Param(w) // was a free param
    case Right(Right(f)) => Param(f.partiallyApply(w))
  }

  def bind(w: FunctionString): Param = p match {
    case Left(_) => this // already bound
    case Right(Left(_)) => Param(w) // was a free param
    case Right(Right(f)) => Param(f.partiallyApply(w))
  }

  def isBound: Boolean = p.isLeft || p.isRight && right.isRight && right.right.get.arity==0

  private def right = p.right.get

  def isFree: Boolean = !isBound && right.isLeft

  //  private def isFunction: Boolean = !isBound && right.isRight

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

  // CONSIDER using use assert instead
  def asserting[A](b: => Boolean, w: => String, f: => A): A = if (b) f else throw AssertingError(w)

  def apply[R: ClassTag](arity: Int, func: (Product) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = RenderableFunction(arity, FunctionString(w, arity, cbn), cbn)(func)

  /**
    * The following apply functions are in pairs: one with FunctionString and one with just String as the second parameter.
    *
    * @param f a function of some mix of T resulting in R
    * @param w a String or a FunctionString
    * @tparam R the result type
    * @return a new RenderableFunction
    */
  def apply[R: ClassTag](f: () => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = {
    asserting(f != null && w != null, "f or w is null",
      apply(0, w, cbn)(asTupledFunctionType(f))
    )
  }

  def apply[R: ClassTag](f: () => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 1, cbn), cbn)

  def apply[T, R: ClassTag](f: T => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = {
    asserting(f != null && w != null, "f or w is null",
      apply(1, w, cbn)(asTupledFunctionType(f))
    )
  }

  def apply[T, R: ClassTag](f: T => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 1, cbn), cbn)

  def apply[T1, T2, R: ClassTag](f: (T1, T2) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(2, w, cbn)(asTupledFunctionType(f))
  )

  def apply[T1, T2, R: ClassTag](f: (T1, T2) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 2, cbn), cbn)

  def apply[T1, T2, T3, R: ClassTag](f: (T1, T2, T3) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(3, w, cbn)(asTupledFunctionType(f))
  )

  def apply[T1, T2, T3, R: ClassTag](f: (T1, T2, T3) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 3, cbn), cbn)

  def apply[T1, T2, T3, T4, R: ClassTag](f: (T1, T2, T3, T4) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(4, w, cbn)(asTupledFunctionType(f))
  )

  def apply[T1, T2, T3, T4, R: ClassTag](f: (T1, T2, T3, T4) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 4, cbn), cbn)

  def apply[T1, T2, T3, T4, T5, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(5, w, cbn)(asTupledFunctionType(f))
  )

  def apply[T1, T2, T3, T4, T5, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 5, cbn), cbn)

  def apply[T1, T2, T3, T4, T5, T6, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(6, w, cbn)(asTupledFunctionType(f))
  )

  def apply[T1, T2, T3, T4, T5, T6, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 6, cbn), cbn)

  def apply[T1, T2, T3, T4, T5, T6, T7, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(7, w, cbn)(asTupledFunctionType(f))
  )

  def apply[T1, T2, T3, T4, T5, T6, T7, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 7, cbn), cbn)

  private def emptyList[T](p: Product) = Seq[T]()

  private val mkList = "mkList"

  def callByName(n: Int): Seq[Boolean] = Stream.continually(true) take n

  def callByValue(n: Int): Seq[Boolean] = Stream.continually(false) take n

  def varargs[T](n: Int): RenderableFunction[Seq[T]] = n match {
    case 0 => apply(0, emptyList, mkList, callByValue(n))
    case 1 => apply({ t1: T => Seq[T](t1) }, mkList, callByValue(n))
    case 2 => apply({ (t1: T, t2: T) => Seq[T](t1, t2) }, mkList, callByValue(n))
    case 3 => apply({ (t1: T, t2: T, t3: T) => Seq[T](t1, t2, t3) }, mkList, callByValue(n))
    case 4 => apply({ (t1: T, t2: T, t3: T, t4: T) => Seq[T](t1, t2, t3, t4) }, mkList, callByValue(n))
    case 5 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T) => Seq[T](t1, t2, t3, t4, t5) }, mkList, callByValue(n))
    case _ => throw RenderableFunctionException(s"varargs of $n is not implemented")
  }

  def asFunctionType[R](f: Function[_, R]): Product => R = asserting(f != null, "f is null", cast[Product => R](f))

  def asTupledFunctionType[R](f: () => R): Product => R = asserting(f != null, "f is null", asFunctionType[R]({ _: Product => f() }))

  def asTupledFunctionType[T1, R](f: T1 => R): Product => R = asserting(f != null, "f is null", asFunctionType[R]({ x: Tuple1[T1] => f(x._1) }))

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

  private def getPartialApplicationFunction[T, R: ClassTag](n: Int, f: (Product) => R, t: T, cbn: Boolean): (Product) => R =
    if (cbn) getPartialApplicationFunction(n, f, { () => t }, false)
    else
      n match {
        case 0 =>
          val f1: Tuple1[T] => R = f
          val g0: (Product) => R = { _: Product => f1(Tuple1(t)) }
          g0
        case 1 =>
          val f2 = Function.untupled[T, Any, R](f)
          val g1 = RenderableFunction.asFunctionType { x: Tuple1[_] => f2.curried(t)(x._1) }
          g1
        case 2 =>
          val f3 = Function.untupled[T, Any, Any, R](f)
          val g2 = RenderableFunction.asFunctionType { x: (_, _) => f3.curried(t)(x._1)(x._2) }
          g2
        case 3 =>
          val f4 = Function.untupled[T, Any, Any, Any, R](f)
          val g3 = RenderableFunction.asFunctionType { x: (_, _, _) => f4.curried(t)(x._1)(x._2)(x._3) }
          g3
        case 4 =>
          val f5 = Function.untupled[T, T, T, T, T, R](f)
          val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f5.curried(t)(x._1)(x._2)(x._3)(x._4) }
          g4
        case 5 =>
          val f6 = FP.untupled[T, T, T, T, T, T, R](f)
          val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f6.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5) }
          g5
        case 6 =>
          val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
          val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
          g6
        case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
      }

  private def getPartialApplicationFunctionCallByName[T, R: ClassTag](n: Int, f: (Product) => R, t: () => T): (Product) => R =
    n match {
      case 0 =>
        val f1: Tuple1[T] => R = f
        val g0: (Product) => R = { _: Product => f1(Tuple1(t())) }
        g0
      case 1 =>
        val f2 = Function.untupled[T, Any, R](f)
        val g1 = RenderableFunction.asFunctionType { x: Tuple1[_] => f2.curried(t())(x._1) }
        g1
      case 2 =>
        val f3 = Function.untupled[T, Any, Any, R](f)
        val g2 = RenderableFunction.asFunctionType { x: (_, _) => f3.curried(t())(x._1)(x._2) }
        g2
      case 3 =>
        val f4 = Function.untupled[T, Any, Any, Any, R](f)
        val g3 = RenderableFunction.asFunctionType { x: (_, _, _) => f4.curried(t())(x._1)(x._2)(x._3) }
        g3
      case 4 =>
        val f5 = Function.untupled[T, T, T, T, T, R](f)
        val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f5.curried(t())(x._1)(x._2)(x._3)(x._4) }
        g4
      case 5 =>
        val f6 = FP.untupled[T, T, T, T, T, T, R](f)
        val g5 = RenderableFunction.asFunctionType { x: (T, T, T, T, T) => f6.curried(t())(x._1)(x._2)(x._3)(x._4)(x._5) }
        g5
      case 6 =>
        val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
        val g6 = RenderableFunction.asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t())(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
        g6
      case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
    }

  private def partiallyApply[T, R: ClassTag](n: Int, f: (Product) => R, t: T, w: FunctionString, cbn: Seq[Boolean]): Try[RenderableFunction[R]] = Try {
    val g0 = getPartialApplicationFunction(n - 1, f, t, cbn.head)
    //    val s = w.partiallyApply(t)
    RenderableFunction(n - 1, w, cbn.tail)(g0)
  }

  private def partiallyApplyFunction[T, R: ClassTag](n: Int, f: (Product) => R, tf: () => T, w: FunctionString, cbn: Seq[Boolean]): Try[RenderableFunction[R]] = Try {
    val g0 = getPartialApplicationFunctionCallByName(n, f, tf)
    RenderableFunction(n, w, cbn.tail)(g0)
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
          RenderableFunction(n, w.invert(n), cbni)(g2)
        case 3 =>
          val f3 = FP.untupled[T, T, T, R](f).curried
          val f3i = n match {
            case 2 => FP.invert2(f3)
            case 3 => FP.invert3(f3)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g3 = RenderableFunction.asFunctionType { x: (T, T, T) => f3i(x._1)(x._2)(x._3) }
          RenderableFunction(n, w.invert(n), cbni)(g3)
        case 4 =>
          val f4 = FP.untupled[T, T, T, T, R](f).curried
          val f4i = n match {
            case 2 => FP.invert2(f4)
            case 3 => FP.invert3(f4)
            case 4 => FP.invert4(f4)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g4 = RenderableFunction.asFunctionType { x: (T, T, T, T) => f4i(x._1)(x._2)(x._3)(x._4) }
          RenderableFunction(n, w.invert(n), cbni)(g4)
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
          RenderableFunction(n, w.invert(n), cbni)(g5)
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
          RenderableFunction(n, w.invert(n), cbni)(g6)
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

  private def nones = Iterator.continually(None)
}

/**
  * RenderableFunctionException
  *
  * @param s the message
  */
case class RenderableFunctionException(s: String) extends Exception(s)

case class AssertingError(w: String) extends Exception(w)

