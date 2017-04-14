package com.phasmid.laScala.fp

/**
  * Created by scalaprof on 8/22/16.
  */
trait Lazy[T] extends (() => T) {

  def unit[U](x: => U): Lazy[U]

  def map[U](f: T => U): Lazy[U] = flatMap(t => unit(f(t)))

  def flatMap[U](f: T => Lazy[U]): Lazy[U]

  def foreach[U](f: T => U): Unit

  def viable: Boolean
}

//case object Deceased extends AtomicLazyNumber[Nothing](null.asInstanceOf[Nothing]) {
//  override def viable = false
//}
//
//case class AtomicLazyNumber[T](x: T) extends Lazy[T] {
//  self =>
//
//  override def apply(): T = x
//
//  def flatMap[U](f: T=>Lazy[U]): Lazy[U] = FlatMappedLazy[U](f)
//  def unit[U](x: U): Lazy[U] = AtomicLazyNumber(x)
//  def filter(p: T => Boolean) = if (p(x)) this else Deceased
//  def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)
//  def viable = true
//
//  def foreach[U](f: (T) => U): Unit = f(x)
//
//  case class FlatMappedLazy[U](g: (T) => Lazy[U]) extends Lazy[U] {
//    def get: Lazy[U] = g(x)
//    def unit[V](u: U): FlatMappedLazy[V] = FlatMappedLazy[V](u,identity)
//    def flatMap[V](f: (U) => Lazy[V]): Lazy[V] = FlatMappedLazy[V](g(x),f)
//  }
//
//  class WithFilter(p: T => Boolean) {
//    def map[U](f: T => U): Lazy[U] = self filter p map f
//    def flatMap[U](f: T => Lazy[U]): Lazy[U] = self filter p flatMap f
//    def foreach[U](f: T => U): Unit = self filter p foreach f
//    def withFilter(q: T => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
//  }
//}





