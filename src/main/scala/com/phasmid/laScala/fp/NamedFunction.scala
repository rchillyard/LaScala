/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

/**
  * This module contains extensible types which can be used to render functions with the toString method.
  *
  * There are some similarities with RenderableFunction, but this module is much simpler and less flexible.
  *
  */

/**
  * This trait defines the concept of a named object, i.e. having a name which is a String.
  *
  */
trait Named {
  /**
    * Provide the name of an object, primarily for rendering/debugging purposes.
    *
    * @return the name
    */
  def name: String

  override def toString: String = name

  val sCurried: String = s"$name!!!"
  val sTupled: String = s"$name###"
}


/**
  * NamedFunction class. Could equally be called NamedFunction1
  *
  * @param name the name of this function
  * @param f    the function itself
  * @tparam T the input type
  * @tparam R the result type
  */
case class NamedFunction[-T, +R](name: String, f: T => R) extends (T => R) {
  override def apply(t: T) = f(t)

  override def toString: String = NamedFunction.toString(name, 1)

  override def compose[A](g: (A) => T): A => R = g match {
    case NamedFunction(w, gf) => NamedFunction(sComposed(w), f.compose(gf))
    case _ => NamedFunction(sComposedDefault(g), f.compose(g))
  }

  override def andThen[A](g: (R) => A): T => A = g match {
    case NamedFunction(w, gf) => NamedFunction(sComposed(w), f.andThen(gf))
    case _ => NamedFunction(sComposedDefault(g), f.andThen(g))
  }


  private def sComposed(s: String) = s"$name&&&$s"
  private def sComposedDefault[P,Q](g: (P)=>Q) = s"$name&&&$g"
}

/**
  * NamedFunction0 class.
  *
  * @param name the name of this function
  * @param f    the function itself
  * @tparam R the result type
  */
case class NamedFunction0[+R](name: String, f: () => R) extends (() => R) with Named {
  override def apply() = f()

  override def toString: String = NamedFunction.toString(name, 0)
}

/**
  * NamedFunction2 class.
  *
  * @param name the name of this function
  * @param f    the function itself
  * @tparam T1 the input type
  * @tparam T2 the input type
  * @tparam R  the result type
  */
case class NamedFunction2[-T1, -T2, +R](name: String, f: (T1, T2) => R) extends ((T1, T2) => R) with Named {
  override def apply(t1: T1, t2: T2) = f(t1, t2)

  override def toString: String = NamedFunction.toString(name, 2)

  override def curried = NamedFunction(sCurried, f.curried)

  override def tupled = NamedFunction(sTupled, f.tupled)
}

/**
  * NamedFunction3 class.
  *
  * @param name the name of this function
  * @param f    the function itself
  * @tparam T1 the input type
  * @tparam T2 the input type
  * @tparam T3 the input type
  * @tparam R  the result type
  */
case class NamedFunction3[-T1, -T2, -T3, +R](name: String, f: (T1, T2, T3) => R) extends ((T1, T2, T3) => R) with Named {
  override def apply(t1: T1, t2: T2, t3: T3) = f(t1, t2, t3)

  override def toString: String = NamedFunction.toString(name, 3)

  override def curried = NamedFunction(sCurried, f.curried)

  override def tupled = NamedFunction(sTupled, f.tupled)
}

/**
  * NamedFunction3 class.
  *
  * @param name the name of this function
  * @param f    the function itself
  * @tparam T1 the input type
  * @tparam T2 the input type
  * @tparam T3 the input type
  * @tparam T4 the input type
  * @tparam R  the result type
  */
case class NamedFunction4[-T1, -T2, -T3, -T4, +R](name: String, f: (T1, T2, T3, T4) => R) extends ((T1, T2, T3, T4) => R) with Named {
  override def apply(t1: T1, t2: T2, t3: T3, t4: T4) = f(t1, t2, t3, t4)

  override def toString: String = NamedFunction.toString(name, 4)

  override def curried = NamedFunction(sCurried, f.curried)

  override def tupled = NamedFunction(sTupled, f.tupled)
}

/**
  * NamedFunction3 class.
  *
  * @param name the name of this function
  * @param f    the function itself
  * @tparam T1 the input type
  * @tparam T2 the input type
  * @tparam T3 the input type
  * @tparam T4 the input type
  * @tparam T5 the input type
  * @tparam R  the result type
  */
case class NamedFunction5[-T1, -T2, -T3, -T4, -T5, +R](name: String, f: (T1, T2, T3, T4, T5) => R) extends ((T1, T2, T3, T4, T5) => R) with Named {
  override def apply(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) = f(t1, t2, t3, t4, t5)

  override def toString: String = NamedFunction.toString(name, 5)

  override def curried = NamedFunction(sCurried, f.curried)

  override def tupled = NamedFunction(sTupled, f.tupled)
}

object NamedFunction {
  def toString(name: String, arity: Int): String = s"<function$arity: $name>"

}

object NamedFunction0 {
}

object NamedFunction2 {
}

object NamedFunction3 {
}

object NamedFunction4 {
}

object NamedFunction5 {
}
