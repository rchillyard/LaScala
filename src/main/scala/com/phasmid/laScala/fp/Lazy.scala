package com.phasmid.laScala.fp

/**
  * Created by scalaprof on 8/22/16.
  */
trait Lazy[T] extends (()=>T)  {
  def unit[U](x: U): Lazy[U]
  def map[U](f: T=>U): Lazy[U] = flatMap(t => unit(f(t)))
  def flatMap[U](f: T=>Lazy[U]): Lazy[U]
}


//case class AtomicLazyNumber[T : Fractional](x: T) extends Lazy[T] {
//  def flatMap[U](f: T=>Lazy[U]): Lazy[U] = FlatMappedLazy[T,U](x,f)
//  def unit[U](x: U): Lazy[U] = x match {
//    case f: Fractional[U] => AtomicLazyNumber(x)(f)
//    case _ => throw new Exception(s"cannot create AtomicLazyNumber from $x")
//  }
//}
//
//case class FlatMappedLazy[T, U](x: T, g: (T) => Lazy[U]) extends Lazy[U] {
//  def unit[V](x: V): Lazy[V] = x match {
//    case f: Fractional[V] => AtomicLazyNumber(x)(f)
//    case _ => throw new Exception(s"cannot create AtomicLazyNumber from $x")
//  }
//
//
//  def flatMap[V](f: (V) => Lazy[V]): Lazy[V] = FlatMappedLazy[U,V](x,f)
//}



