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
  * @param arity the number of input parameters that are required to fully apply the given function (f)
  * @param w     a human-readable representation of the function
  * @param cbn   a sequence of booleans, corresponding to each of the parameters of (untupled) func. If the boolean is true, then that parameter is call-by-name, i.e. a Function0[T] rather than a T.
  * @param cs    a sequence of ClassTags corresponding to the parameters to be passed to f
  * @param f     the function itself (this field is not considered in the hashCode and equals methods)
  * @tparam R the ultimate return type of this RenderableFunction (must support ClassTag)
  */
case class RenderableFunction[R: ClassTag](arity: Int, w: FunctionString, cbn: Seq[Boolean], cs: ParamClasses)(f: (Product) => R) extends (Product => Try[R]) with Renderable with Serializable {

  implicit private val logger = Spy.getLogger(getClass)

  require(f != null, s"f is null")
  require(w != null, s"w is null")
  require(w.arity == arity, s"arity $arity is not consistent with $w")
  require(cbn.size == arity, s"cbn sequence: has size ${cbn.size} which is not equal to $arity")

  private val rc = implicitly[ClassTag[R]]
  private val returnIsTry = rc.runtimeClass == classOf[Try[_]]


  /**
    * Apply parameter p to this function.
    *
    * NOTE: that if R is Try[S], then the result of apply will be R, not Try[R]
    *
    * @param p a Tuple
    * @return an R wrapped in Try
    */
  def apply(p: Product): Try[R] =
    if (arity == p.productArity) {
      val g: (Product) => R = func
      val ry = RenderableFunction.unwrapIfNecessary(Try(g(p)), returnIsTry, s"apply($p): logic error")
      for (r <- ry) yield RenderableFunction.toScala[R](r)
    }
    else
      Failure(RenderableFunctionException(s"cannot apply Product of arity ${p.productArity}: $p to RenderableFunction with arity $arity: ($this)"))

  /**
    * Apply parameter p to this function.
    *
    * @param t a T
    * @return the result of invoking apply(Tuple1(t))
    */
  def apply[T](t: T): Try[R] = if (arity == 1) Spy.spy(s"$this.apply($t)", apply(Tuple1(t)))
  else throw RenderableFunctionException(s"apply($t): arity is not one ($arity) for $this")

  /**
    * Method to call this RenderableFunction by name only, that's to say without any parameters.
    * If this is a 0-arity function, then Success(r) will be returned. Otherwise, you will get a failure.
    *
    * @return
    */
  def callByName(): Try[R] = if (arity == 0) Spy.spy(s"$this.callByName", apply(Tuple0))
  else throw RenderableFunctionException(s"callByName(): arity is not zero ($arity) for $this")

  /**
    * Method to get the function that is the basis of this RenderableFunction
    *
    * @return the function as a Product=>R
    */
  def func: (Product) => R = f

  /**
    * Method which will take this RenderableFunction and apply the given parameters, except that any Closure parameters will themselves be partiallyApplied.
    * The result is a RenderableFunction[R] wrapped in Try.
    *
    * @param tps the list of parameters
    * @return a RenderableFunction[R] (wrapped in Try)
    */
  def partiallyApplyParameters(tps: Seq[Parameter[_]]): Try[RenderableFunction[R]] = {
    def partiallyApplyParameter(rfy: Try[RenderableFunction[R]], tp: Parameter[_], x: ClassTag[_]): Try[RenderableFunction[R]] = tp match {
      case Left(t) => // t: T
        rfy flatMap (_.partiallyApply(t)(x.asInstanceOf[ClassTag[Any]]))
      case Right(c) => // c: Closure[_,_]
        for (g <- c.partiallyApply; f = g.f; rf <- rfy; h <- rf.partiallyApplyFunction(f.asFunction, f.w)) yield h
    }

    @tailrec
    def inner(rfy: Try[RenderableFunction[R]], work: Seq[(Parameter[_], ClassTag[_])]): Try[RenderableFunction[R]] =
      work match {
        case Nil => rfy
        case (tp, c) :: _tps => inner(partiallyApplyParameter(rfy, tp, c), _tps)
      }

    inner(Try(this), tps zip cs)
  }

  /**
    * Convert this RenderableFunction into a pure, parameterless function.
    *
    * @return
    */
  def asFunction: () => R = if (arity == 0)
    () => {
      val f = func(Tuple0)
      f match {
        case r: R => r
        case g: Function[Unit, _]@unchecked => RenderableFunction.toScala[R](g(()))
        // NOTE: this could result in the throwing of an exception
        case x => RenderableFunction.toScala[R](x)
      }
    }
  else throw RenderableFunctionException(s"asFunction: arity is not zero ($arity) for $this")

  /**
    * Method to partially apply the value t to this RenderableFunction.
    * In this context (t comes from a Parameter which is a Left[T]), then x is always a T (never a function).
    *
    * @param t a value which should yield a T
    * @tparam T the desired type
    * @return RenderableFunction[R] wrapped in Try
    */
  def partiallyApply[T: ClassTag](t: Any): Try[RenderableFunction[R]] =
    RenderableFunction.partiallyApply(arity, func, t.asInstanceOf[T], w, cbn, cs)

  /**
    * Method to partially apply the function f to this RenderableFunction.
    *
    * CONSIDER inline this method
    *
    * @param f the function
    * @param z the corresponding FunctionString
    * @tparam T the return type of f
    * @return a RenderableFunction[R] wrapped in Try
    */
  def partiallyApplyFunction[T](f: () => T, z: FunctionString): Try[RenderableFunction[R]] =
    RenderableFunction.partiallyApplyFunction(arity, func, f, w.partiallyApplyFunction(z), cbn, cs)

  /**
    * Method to form a String (for debugging purposes) from this RenderableFunction
    *
    * NOTE that we don't show the function itself because it gives us little additional information (only the actual arity of the function)
    *
    * @return a String representing this object
    */
  override def toString = s"RenderableFunction[$cs,$rc]($arity, $evaluationStyle $w)"

  /**
    * Invert the first n parameter positions of this RenderableFunction
    *
    * @param n the number of parameters to invert
    * @tparam T the underling type of the parameters
    * @return a new RenderableFunction with the parameters inverted
    */
  def invert[T](n: Int): RenderableFunction[R] = RenderableFunction.invert(n, arity, func, w, cbn, cs)

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
    * XXX in practice, the only real use for this, currently, is method evaluationStyle
    *
    * @return if true, then this function can be eagerly evaluated.
    */
  def alwaysEvaluate: Boolean = cbn.forall(!_)

  private def evaluationStyle = if (alwaysEvaluate) "" else "=>"
}

/**
  * Class to represent a (renderable) function string.
  *
  * @param f  the name of the function (not necessarily the same as the original definition of the corresponding function/method)
  * @param ps a sequence of Param objects (these represent bound or unbound parameters)
  */
case class FunctionString(f: String, ps: Seq[Param]) {
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
      FunctionString(f, bind(bound + skip, w))
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

    inner(0, ps)
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

    inner(0, ps)
  }

  def bind(index: Int, w: String): Seq[Param] = bind(FunctionString.nones.take(index).toSeq :+ Some(Left(w)))

  def bind(index: Int, w: FunctionString): Seq[Param] = bind(FunctionString.nones.take(index).toSeq :+ Some(Right(w)))

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

    inner(Nil, ps zipAll(wos, null, None))
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

  def toScala[T: ClassTag](t: Any): T = (t match {
    case q: java.lang.Boolean => Boolean.unbox(q)
    case q: java.lang.Integer => Int.unbox(q)
    case q: java.lang.Double => Double.unbox(q)
    case _ => t
  }).asInstanceOf[T]

  /**
    * This is similar to the toScala method except that it isn't concerned with converting Java types to Scala.
    * For now, there is no attempt at conversion unless T is a String.
    * If the value cannot be converted, an exception will be thrown.
    *
    * @param x an Any
    * @tparam T the return type
    * @return a T
    */
  def convert[T: ClassTag](x: Any): T = x match {
    case t: T => t
    case _ =>
      val tc = implicitly[ClassTag[T]]
      if (tc.runtimeClass == classOf[String]) x.toString.asInstanceOf[T]
      else throw RenderableFunctionException(s"$x of type ${x.getClass} cannot be converted to $tc")
  }

  // CONSIDER using use assert instead
  def asserting[A](b: => Boolean, w: => String, f: => A): A = if (b) f else throw AssertingError(w)

  def apply[R: ClassTag](arity: Int, func: (Product) => R, w: String, cbn: Seq[Boolean], cs: ParamClasses): RenderableFunction[R] = RenderableFunction(arity, FunctionString(w, arity, cbn), cbn, cs)(func)

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
      apply(0, w, cbn, Nil)(asTupledFunctionType(f))
    )
  }

  def apply[R: ClassTag](f: () => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 1, cbn), cbn)

  def apply[T: ClassTag, R: ClassTag](f: T => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = {
    asserting(f != null && w != null, "f or w is null",
      apply(1, w, cbn, Seq(implicitly[ClassTag[T]]))(asTupledFunctionType(f))
    )
  }

  def apply[T: ClassTag, R: ClassTag](f: T => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 1, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, R: ClassTag](f: (T1, T2) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(2, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, R: ClassTag](f: (T1, T2) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 2, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, R: ClassTag](f: (T1, T2, T3) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(3, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, R: ClassTag](f: (T1, T2, T3) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 3, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, R: ClassTag](f: (T1, T2, T3, T4) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(4, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]], implicitly[ClassTag[T4]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, R: ClassTag](f: (T1, T2, T3, T4) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 4, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(5, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]], implicitly[ClassTag[T4]], implicitly[ClassTag[T5]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 5, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(6, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]], implicitly[ClassTag[T4]], implicitly[ClassTag[T5]], implicitly[ClassTag[T6]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 6, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(7, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]], implicitly[ClassTag[T4]], implicitly[ClassTag[T5]], implicitly[ClassTag[T6]], implicitly[ClassTag[T7]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 7, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(8, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]], implicitly[ClassTag[T4]], implicitly[ClassTag[T5]], implicitly[ClassTag[T6]], implicitly[ClassTag[T7]], implicitly[ClassTag[T8]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 8, cbn), cbn)

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, w: FunctionString, cbn: Seq[Boolean]): RenderableFunction[R] = asserting(f != null && w != null, "f or w is null",
    apply(9, w, cbn, Seq(implicitly[ClassTag[T1]], implicitly[ClassTag[T2]], implicitly[ClassTag[T3]], implicitly[ClassTag[T4]], implicitly[ClassTag[T5]], implicitly[ClassTag[T6]], implicitly[ClassTag[T7]], implicitly[ClassTag[T8]], implicitly[ClassTag[T9]]))(asTupledFunctionType(f))
  )

  def apply[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, R: ClassTag](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, w: String, cbn: Seq[Boolean]): RenderableFunction[R] = apply(f, FunctionString(w, 9, cbn), cbn)


  def callByName(n: Int): Seq[Boolean] = Stream.continually(true) take n

  def callByValue(n: Int): Seq[Boolean] = Stream.continually(false) take n

  /**
    * Method to create a varargs function.
    *
    * NOTE: this is limited in the value of n which it can support (currently 7)
    *
    * NOTE: there is a far superior way of dealing with varargs: Closure.createVarArgsClosure
    *
    * @see Closure
    * @param n the number of args: a number less than or equal to 7
    * @tparam T the type of the parameters
    * @return a RenderableFunction which yields a Seq[T]
    */
  def varargs[T: ClassTag](n: Int): RenderableFunction[Seq[T]] = {
    val mkList = "mkList"
    val cbn = callByValue(n)
    val cs: ParamClasses = Stream.continually(implicitly[ClassTag[T]].asInstanceOf[ClassTag[Any]]) take n

    def emptyList(p: Product) = Seq[T]()

    /**
      * Method to take a product of any arity and return a Seq[T]
      * Any element of the given product which is no a T,
      *
      * @param p a Product (or Tuple)
      * @return a Seq[T]
      */
    def f(p: Product): Seq[T] = (for (x <- p.productIterator) yield convert(x)).toSeq

    n match {
      case 0 => apply(0, emptyList, mkList, cbn, cs)
      case 1 => apply({ t1: T => Seq[T](t1) }, mkList, cbn)
      case 2 => apply({ (t1: T, t2: T) => Seq[T](t1, t2) }, mkList, cbn)
      case 3 => apply({ (t1: T, t2: T, t3: T) => Seq[T](t1, t2, t3) }, mkList, cbn)
      case 4 => apply({ (t1: T, t2: T, t3: T, t4: T) => Seq[T](t1, t2, t3, t4) }, mkList, cbn)
      case 5 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T) => Seq[T](t1, t2, t3, t4, t5) }, mkList, cbn)
      case 6 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T, t6: T) => Seq[T](t1, t2, t3, t4, t5, t6) }, mkList, cbn)
      case 7 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T) => Seq[T](t1, t2, t3, t4, t5, t6, t7) }, mkList, cbn)
      case 8 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T, t8: T) => Seq[T](t1, t2, t3, t4, t5, t6, t7, t8) }, mkList, cbn)
      case 9 => apply({ (t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T, t8: T, t9: T) => Seq[T](t1, t2, t3, t4, t5, t6, t7, t8, t9) }, mkList, cbn)
      case _ => apply(n, FunctionString(mkList, n), cbn, cs)(f)
    }
  }

  def tupled[T1: ClassTag, R: ClassTag](f: T1 => R): Tuple1[T1] => R = {
    case Tuple1(x1) =>
      if (f.isInstanceOf[(T1) => R]) f(x1)
      else throw RenderableFunctionException(s"$f is not a Function1[${implicitly[ClassTag[T1]]},${implicitly[ClassTag[R]]}]")
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

  def asTupledFunctionType[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): Product => R = asserting(f != null, "f is null", asFunctionType[R](f.tupled))

  def untupled[T, R](f: Tuple1[T] => R): T => R = asserting(f != null, "f is null", { x => f(Tuple1(x)) })

  private def unwrapIfNecessary[R](ry: Try[R], isTry: Boolean, s: String) = if (isTry) unwrap(ry, s) else ry

  private def unwrap[R](ry: Try[R], msg: String): Try[R] = ry match {
    case Success(r: Try[R]@unchecked) => r
    case Failure(x) => Failure(x)
    case _ => Failure(RenderableFunctionException(msg))
  }

  private def cast[R: ClassTag](x: Any): R = x match {
    case r: R => r
    case _ => throw new ClassCastException(s"x: $x of type ${x.getClass} cannot be cast to type ${implicitly[ClassTag[R]]}")
  }

  /**
    * This method unwraps x if it is a Try[T] otherwise, it assumes x to be a T.
    *
    * CONSIDER refactoring so that this method is not necessary.
    * (Exactly why we need this method -- when we already have apply(Product) to take care of this, I don't know).
    *
    * @param x a value of any type
    * @tparam T the desired result type
    * @return a value of type T
    * @throws RenderableFunctionException if unable to properly for a T value from x
    */
  private def extract[T: ClassTag](x: Any): T = x match {
    case Success(t: T) => t
    case Failure(e) => throw e
    case t: T => t
    case t =>
      try RenderableFunction.toScala[T](t)
      catch {
        case z: Exception => throw RenderableFunctionException(s"cannot extract a ${implicitly[ClassTag[T]]} value from $x", z)
      }
  }

  /**
    * Used by partiallyApplyParameters in RenderableFunction class
    */
  /**private*/ def partiallyApply[T, R: ClassTag](n: Int, f: (Product) => R, t: T, w: FunctionString, cbn: Seq[Boolean], cs: ParamClasses): Try[RenderableFunction[R]] = Try {
    val g = getPartialApplicationFunction(n, f, t, cbn.head)(cs.head.asInstanceOf[ClassTag[T]], implicitly[ClassTag[R]])
    RenderableFunction(n-1, w.partiallyApply(t), cbn.tail, cs.tail)(g)
  }

  /**
    * Used by partiallyApplyParameters, via partiallyApplyFunction.
    * Function f has
    *
    * @return RenderableFunction wrapped in Try
    */
  private def partiallyApplyFunction[T, R: ClassTag](n: Int, f: (Product) => R, tf: () => T, w: FunctionString, cbn: Seq[Boolean], cs: ParamClasses): Try[RenderableFunction[R]] = Try {
    val g = if (cbn.head)
      getPartialApplicationFunction(n, f, tf, cbn = false)(cs.head.asInstanceOf[ClassTag[() => T]], implicitly[ClassTag[R]])
    else
      getPartialApplicationFunctionCallByName(n, f, tf)(cs.head.asInstanceOf[ClassTag[T]], implicitly[ClassTag[R]])
    RenderableFunction(n - 1, w, cbn.tail, cs.tail)(g)
  }

  /**
    * This method is used to partially apply the given function (f), with the given parameter (t) and yield
    * a function with arity one less than that of f.
    *
    * @param n the arity of f
    * @param f the function to be partially applied
    * @param t the parameter to be applied to f
    * @param cbn if true then the parameter is satisfied via call-by-name
    * @tparam T the type of t
    * @tparam R the type of the result of invoking f
    * @return a function of arity n-1 which is the result of applying t to f
    */
  // private
  def getPartialApplicationFunction[T: ClassTag, R: ClassTag](n: Int, f: (Product) => R, t: T, cbn: Boolean): (Product) => R =
  if (cbn) getPartialApplicationFunction(n, f, { () => t }, false)
  else
    n match {
      case 1 =>
        asFunctionType { _: Product => f(Tuple1(t)) }
      case 2 =>
        val f2 = Function.untupled[T, Any, R](f)
        asFunctionType { x: Tuple1[_] => f2.curried(t)(x._1) }
      case 3 =>
        val f3 = Function.untupled[T, Any, Any, R](f)
        asFunctionType { x: (_, _) => f3.curried(t)(x._1)(x._2) }
      case 4 =>
        val f4 = Function.untupled[T, Any, Any, Any, R](f)
        asFunctionType { x: (_, _, _) => f4.curried(t)(x._1)(x._2)(x._3) }
      case 5 =>
        val f5 = Function.untupled[T, T, T, T, T, R](f)
        asFunctionType { x: (T, T, T, T) => f5.curried(t)(x._1)(x._2)(x._3)(x._4) }
      case 6 =>
        val f6 = FP.untupled[T, T, T, T, T, T, R](f)
        asFunctionType { x: (T, T, T, T, T) => f6.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5) }
      case 7 =>
        val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
        asFunctionType { x: (T, T, T, T, T, T) => f7.curried(t)(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
      case _ =>
        throw RenderableFunctionException(s"cannot partially apply function of arity $n (not supported/implemented)")
    }

  /**
    * Called by PartiallyApplyParameters (via class.partiallyApplyParameter and object.partiallyApplyFunction)
    */
  private def getPartialApplicationFunctionCallByName[T: ClassTag, R: ClassTag](n: Int, f: (Product) => R, t: () => T): (Product) => R =
    n match {
      case 1 =>
        val f1: Tuple1[T] => R = f
        // CONSIDER see why we have to call extract. It should never be necessary.
        val g0: (Product) => R = { _: Product => f1(Tuple1(extract(t())(implicitly[ClassTag[T]]))) }
        g0
      case 2 =>
        val f2 = Function.untupled[T, Any, R](f)
        val g1 = asFunctionType { x: Tuple1[_] => f2.curried(extract(t())(implicitly[ClassTag[T]]))(x._1) }
        g1
      case 3 =>
        val f3 = Function.untupled[T, Any, Any, R](f)
        val g2 = asFunctionType { x: (_, _) => f3.curried(extract(t())(implicitly[ClassTag[T]]))(x._1)(x._2) }
        g2
      case 4 =>
        val f4 = Function.untupled[T, Any, Any, Any, R](f)
        val g3 = asFunctionType { x: (_, _, _) => f4.curried(extract(t())(implicitly[ClassTag[T]]))(x._1)(x._2)(x._3) }
        g3
      case 5 =>
        val f5 = Function.untupled[T, T, T, T, T, R](f)
        val g4 = asFunctionType { x: (T, T, T, T) => f5.curried(extract(t())(implicitly[ClassTag[T]]))(x._1)(x._2)(x._3)(x._4) }
        g4
      case 6 =>
        val f6 = FP.untupled[T, T, T, T, T, T, R](f)
        val g5 = asFunctionType { x: (T, T, T, T, T) => f6.curried(extract(t())(implicitly[ClassTag[T]]))(x._1)(x._2)(x._3)(x._4)(x._5) }
        g5
      case 7 =>
        val f7 = FP.untupled[T, T, T, T, T, T, T, R](f)
        val g6 = asFunctionType { x: (T, T, T, T, T, T) => f7.curried(extract(t())(implicitly[ClassTag[T]]))(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
        g6
      case _ => throw RenderableFunctionException(s"FunctionCalls with arity $n are not supported")
    }

  private def invert[X](n: Int, xs: Seq[X]): Seq[X] = ((xs take (xs.size - n)) :+ xs(n - 1) :+ xs(n - 2)) ++ (xs drop n)

  private def invert[T, R: ClassTag](n: Int, arity: Int, f: (Product) => R, w: FunctionString, cbn: Seq[Boolean], cs: ParamClasses): RenderableFunction[R] = {
    if (arity < 2 || n > arity)
      throw RenderableFunctionException(s"can't invert $n parameters of a RenderableFunction of arity $arity")
    else {
      val cbni = invert(n, cbn)
      val csi = invert(n, cs)
      arity match {
        case 2 =>
          val f2 = FP.untupled[T, T, R](f).curried
          val f2i = n match {
            case 2 => FP.invert2(f2)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g2 = asFunctionType { x: (T, T) => f2i(x._1)(x._2) }
          RenderableFunction(n, w.invert(n), cbni, csi)(g2)
        case 3 =>
          val f3 = FP.untupled[T, T, T, R](f).curried
          val f3i = n match {
            case 2 => FP.invert2(f3)
            case 3 => FP.invert3(f3)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g3 = asFunctionType { x: (T, T, T) => f3i(x._1)(x._2)(x._3) }
          RenderableFunction(n, w.invert(n), cbni, csi)(g3)
        case 4 =>
          val f4 = FP.untupled[T, T, T, T, R](f).curried
          val f4i = n match {
            case 2 => FP.invert2(f4)
            case 3 => FP.invert3(f4)
            case 4 => FP.invert4(f4)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g4 = asFunctionType { x: (T, T, T, T) => f4i(x._1)(x._2)(x._3)(x._4) }
          RenderableFunction(n, w.invert(n), cbni, csi)(g4)
        case 5 =>
          val f5 = FP.untupled[T, T, T, T, T, R](f).curried
          val f5ci = n match {
            case 2 => FP.invert2(f5)
            case 3 => FP.invert3(f5)
            case 4 => FP.invert4(f5)
            case 5 => FP.invert5(f5)
            case _ => throw RenderableFunctionException(s"cannot invert $n parameters when arity = $arity")
          }
          val g5 = asFunctionType { x: (T, T, T, T, T) => f5ci(x._1)(x._2)(x._3)(x._4)(x._5) }
          RenderableFunction(n, w.invert(n), cbni, csi)(g5)
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
          val g6 = asFunctionType { x: (T, T, T, T, T, T) => f6i(x._1)(x._2)(x._3)(x._4)(x._5)(x._6) }
          RenderableFunction(n, w.invert(n), cbni, csi)(g6)
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
case class RenderableFunctionException(s: String, t: Throwable = null) extends Exception(s, t)

case class AssertingError(w: String) extends Exception(w)

