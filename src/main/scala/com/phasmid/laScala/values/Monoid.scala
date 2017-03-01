/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

/**
  * NOTE This is experimental code.
  *
  * Created by scalaprof on 10/5/16.
  */
trait Monoid[X] {
  def empty: X

  def combine(x: X, y: X): X
}

object Monoid {
  def apply[X: Monoid]: Monoid[X] = implicitly[Monoid[X]]
}

trait Additive[X] {
  def add(x: X, y: X): X
}

// Implementation for Int/Addition
object IntAdditionMonoid extends Monoid[Int] with Additive[Int] {
  def empty: Int = 0

  def combine(x: Int, y: Int): Int = add(x, y)

  def add(x: Int, y: Int): Int = x + y
}

///**
//  * NOTE This is experimental code.
//  *
//  * Created by scalaprof on 10/5/16.
//  */
//trait Monoid[X] extends Incrementable[X] {
//
////  val combiner: (X,X)=>X
////
////  val identity: (()=>X)
//}

//trait Additive[X] {
//  def add(x1: X, x2: X): X
//}
//
//trait Multiplicative[X] {
//  def multiply(x1: X, x2: X): X
//}
//
//object Additive {
//
//  trait AdditiveInt extends Incrementable.IncrementableInt with Additive[Int] {
//    def add(x1: Int, x2: Int): Int = x1+x2
//  }
//  object AdditiveInt extends AdditiveInt
//}
//
//object Multiplicative {
//
//  trait MultiplicativeInt extends Incrementable.IncrementableInt with Multiplicative[Int] {
//    def multiply(x1: Int, x2: Int): Int = x1*x2
//  }
//  object MultiplicativeInt extends MultiplicativeInt
//}
//
//object Monoid {
//
//  trait AdditiveMonoidInt extends Incrementable.IncrementableInt with Monoid[Int] with AdditiveInt {
//    val combiner: (Int, Int) => Int = add
//    val identity: () => Int = {() => 0}
//  }
//
//  implicit object AdditiveMonoidIntMonoid extends AdditiveMonoidInt
//}