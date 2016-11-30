package com.phasmid.laScala.values

import com.phasmid.laScala.values.Additive.AdditiveInt

/**
  * NOTE This is experimental code.
  *
  * Created by scalaprof on 10/5/16.
  */
trait Monoid[X] extends Incrementable[X] {

  val combiner: (X,X)=>X

  val identity: (()=>X)
}

trait Additive[X] {
  def add(x1: X, x2: X): X
}

trait Multiplicative[X] {
  def multiply(x1: X, x2: X): X
}

object Additive {

  trait AdditiveInt extends Incrementable.IncrementableInt with Additive[Int] {
    def add(x1: Int, x2: Int): Int = x1+x2
  }
  object AdditiveInt extends AdditiveInt
}

object Multiplicative {

  trait MultiplicativeInt extends Incrementable.IncrementableInt with Multiplicative[Int] {
    def multiply(x1: Int, x2: Int): Int = x1*x2
  }
  object MultiplicativeInt extends MultiplicativeInt
}

object Monoid {

  trait AdditiveMonoidInt extends Incrementable.IncrementableInt with Monoid[Int] with AdditiveInt {
    val combiner: (Int, Int) => Int = add
    val identity: () => Int = {() => 0}
  }

  implicit object AdditiveMonoidIntMonoid extends AdditiveMonoidInt
}