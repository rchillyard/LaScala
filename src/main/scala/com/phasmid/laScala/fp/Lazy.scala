package com.phasmid.laScala.fp

/**
  * Created by scalaprof on 8/22/16.
  */
trait Lazy[T] extends (()=>T)  {
  def unit[U](x: => U): Lazy[U]
  def map[U](f: T=>U): Lazy[U] = flatMap(t => unit(f(t)))
  def flatMap[U](f: T=>Lazy[U]): Lazy[U]
}


//case class AtomicLazyNumber[T](x: T) extends Lazy[T] {
//  def flatMap[U](f: T=>Lazy[U]): Lazy[U] = FlatMappedLazy[T,U](x,f)
//  def unit[U](x: U): Lazy[U] = AtomicLazyNumber(x)
//}
//
//case class FlatMappedLazy[T, U](x: T, g: (T) => Lazy[U]) extends Lazy[U] {
//  def get: Lazy[U] = g(x)
//  def unit[V](u: U): FlatMappedLazy[U, V] = FlatMappedLazy(u,identity)
//  def flatMap[V](f: (U) => Lazy[V]): Lazy[V] = FlatMappedLazy[U,V](g(x),f)
//}
//


