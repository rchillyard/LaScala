package com.phasmid.laScala.cache

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.{higherKinds, implicitConversions}
import scala.util.{Failure, Success, Try}

/**
  * This trait defines a cache (aka memoization) of K,V key-value pairs.
  * It extends Function1[K,V].
  * If you call the apply method on the cache, it may throw an exception if it is unable to fulfill the value for the given key.
  *
  * @tparam K they key type
  * @tparam V the value type
  */
trait Cache[K,V] extends (K=>V) {

  /**
    * The type of the value returned by get. This could be Option[V], Try[V] or Future[V].
    */
  type T

  /**
    * Method to get the value corresponding to key k as a T
    *
    * @param k the key we are interested in
    * @return the value wrapped as a T
    */
  def get(k: K): T

  /**
    * Method to return the currently cached values as an immutable Map.
    * CONSIDER moving this method into the abstract class
    *
    * @return
    */
  def asMap: Map[K,V]

  /**
    * Method to determine if this cache contains the key k
    * CONSIDER moving this method into the abstract class or eliminating altogether since asMap.contains will yield the answer.
    *
    * @param k the key
    * @return true if this cache contains k else false
    */
  def contains(k: K): Boolean

  /**
    * Method to clear the cache such that there are absolutely no stale entries.
    * Calling clear does NOT refresh the cache as currently programmed.
    * Thus clear never causes the evaluate method to be invoked.
    */
  def clear()
}

trait Expiring[K] {

  /**
    * method to remove a key from the cache
    *
    * @param k the key to the element which is to be expired.
    */
  def expire(k: K): Unit = {}
}

/**
  * Basic self-fulfilling cache that uses Option
  *
  * @param evaluate the evaluate function of type K=>Option[V] which will get the V value when it is otherwise unknown.
  * @param carper the function to carp about a problem, of type String=>Unit
  * @tparam K they key type
  * @tparam V the value type
  */
case class BasicExpiringCache[K,V](evaluate: K=>Option[V])(implicit carper: String=>Unit) extends SelfFulfillingExpiringCache[K,V](evaluate)(carper)

/**
  * Basic self-fulfilling cache that uses Try
  *
  * @param evaluate the evaluate function of type K=>Try[V] which will get the V value when it is otherwise unknown.
  * @param carper the function to carp about a problem, of type String=>Unit
  * @tparam K they key type
  * @tparam V the value type
  */
case class NonExpiringCache[K,V](evaluate: K=>Try[V])(implicit carper: String=>Unit) extends TryFulfillingExpiringCache[K,V](evaluate)(carper)

/**
  * This abstract class implements a (mutable, obviously) Cache (aka Memoization) of K,V key-value pairs.
  * In particular, Cache is a lookup function of type K=>V.
  * If no value v for key k has been previously established,
  * the evaluate method will be called when get(k) -- and so also when apply(k) -- is invoked.
  * Otherwise, the existing value will be returned.
  * If the result of calling evaluate(k) is:
  * a Success(v) then v will be put into the internal hashmap and returned;
  * a Failure(x) then no entry will be put into the internal hashmap
  * (as a side-effect, the carper will be invoked for a failure)
  * In this way, a caller must be prepared to get None as the result of calling get(k) and must be prepared for
  * an exception to be thrown when calling apply(k).
  * The advantage is that we will only ever try to evaluate a key at most once per expiration period.
  *
  * SelfFulfillingExpiringCache defines two methods which must be implemented by subclasses:
  * expire (to remove an element from the cache) and postFulfillment (designed to allow a callback method
  * to be invoked when a value is added to the cache, i.e. it is fulfilled).
  *
  * CONSIDER do we really need the carper method now that we return a Try[V] from apply?
  *
  * Created by scalaprof on 4/5/16.
  */
abstract class BaseExpiringCache[K,V,X[_]](val m: CacheMap[K,V,X])(implicit carper: String=>Unit) extends Cache[K, V] with Expiring[K] {

  /**
    * Return a V value for the given K value
    *
    * @param k the K value we are interested in
    * @return the corresponding V value
    */
  def apply(k: K): V = unwrap(get(k))

  def clear() = m.clear

  def asMap = m.toMap

  def contains(k: K) = m.contains(k)

  /**
    * method to take action whenever a key value is actually fulfilled.
    *
    * @param k the key of the element which has been fulfilled
    * @param v the value of the element which has been fulfilled
    */
  def postFulfillment(k: K, v: V): Unit = {}

  /**
    * Method to unwrap a value of type T.
    *
    * TODO figure out a way to avoid having to use asInstanceOf
    *
    * @param t the T value, which should be the same as an X[V] value
    * @return the unwrapped V value
    */
  def unwrap(t: T): V = m.wrapper.get(t.asInstanceOf[X[V]])

  override def toString = m.toString
}

/**
  * This class implements a (mutable, obviously) Cache (aka Memoization) of K,V key-value pairs.
  * In particular, Cache is a lookup function of type K=>V.
  * If no value v for key k has been previously established,
  * the evaluate method will be called when get(k) -- and so also when apply(k) -- is invoked.
  * Otherwise, the existing value will be returned.
  * If the result of calling evaluate(k) is:
  * a Some(v) then v will be put into the internal hashmap and returned;
  * a None then no entry will be put into the internal hashmap (and, as a side-effect, the carper will be invoked).
  * In this way, a caller must be prepared to get None as the result of calling get(k) and must be prepared for
  * an exception to be thrown when calling apply(k).
  * The advantage is that we will only ever try to evaluate a key at most once per expiration period.
  *
  * SelfFulfillingExpiringCache defines two methods which must be implemented by subclasses:
  * expire (to remove an element from the cache) and postFulfillment (designed to allow a callback method
  * to be invoked when a value is added to the cache, i.e. it is fulfilled).
  *
  * Created by scalaprof on 4/5/16.
  */
abstract class SelfFulfillingExpiringCache[K,V](evaluate: K=>Option[V])(implicit carper: String=>Unit) extends BaseExpiringCache[K,V,Option](new OptionHashMap[K,V]()) {

  /**
    * Define T as Option[V]
    */
  type T = Option[V]

  /**
    * TODO try to define this in BaseExpiringCache
    *
    * @param k the key we are interested in
    * @return the value wrapped as a T
    */
  def get(k: K): T = m.getOrElseUpdateX(k, fulfill _)

  /**
    * CONSIDER making this generic and putting in BaseExpiringCache
    *
    * @param k the key of the element to be fulfilled
    * @return
    */
  private def fulfill(k: K): T = evaluate(k) match {
    case Some(v) => postFulfillment(k,v); Some(v)
    case _ => carper(s"Cache: unable to evaluate value for key $k"); None
  }
}

/**
  * This class is almost exactly like SelfFulfillingExpiringCache but the get method returns a Try[V] rather than a Option[V].
  * There is a corresponding change in the definition of the evaluate function (passed in as a parameter).
  *
  * Created by scalaprof on 4/5/16.
  */
abstract class TryFulfillingExpiringCache[K,V](evaluate: K=>Try[V])(implicit carper: String=>Unit) extends BaseExpiringCache[K,V,Try](new TryHashMap[K,V]()) {

  /**
    * Define T as Try[V]
    */
  type T = Try[V]

  def get(k: K): T = m.getOrElseUpdateX(k, fulfill _)

  private def fulfill(k: K): T = evaluate(k).transform(
    {v => postFulfillment(k,v); Success(v)},
    {case x => carper(s"Cache: evaluation exception for key $k: $x"); Failure(x)}
  )
}

/**
  * This class is almost exactly like SelfFulfillingExpiringCache but the get method returns a Future[V] rather than a Option[V].
  * There is a corresponding change in the definition of the evaluate function (passed in as a parameter).
  *
  * Created by scalaprof on 4/5/16.
  */
abstract class FutureFulfillingExpiringCache[K,V](evaluate: K=>Future[V])(implicit carper: String=>Unit, executor: ExecutionContext) extends BaseExpiringCache[K,V,Future](new FutureHashMap[K,V]()) {
  /**
    * Define T as Future[V]
    */
  type T = Future[V]

  /**
    * Non-blocking call to get the value v from the cache.
    *
    * @param k the key of the element which we wish to get
    * @return the value wrapped as a T
    */
  def get(k: K): T = m.getOrElseUpdateX(k, fulfill _)

  private def fulfill(k: K): T = evaluate(k).transform(
    {v => postFulfillment(k,v); v},
    {case x => carper(s"Cache: evaluation exception for key $k: $x"); x}
  )
}

object Cache {
  implicit def carper(s: String): Unit = println(s)
}

/**
  * This trait defines a Wrapper of values.
  *
  * @tparam M represents a container (which is a monad) that we wish to wrap our values in.
  */
trait Wrapper[M[_]] {
  /**
    * Return an M[V], given v
    *
    * @param v the value
    * @tparam V the value type
    * @return an M[V]
    */
  def unit[V](v: => V): M[V]
  /**
    * Return an M[V], given an M[V] and a V=>M[V] function
    *
    * CONSIDER definining this as flatMap[V,W] etc.
    *
    * @param m the M[V] to be mapped
    * @tparam V the value type
    * @return an M[V]
    */
  def flatMap[V](m: M[V], f: V=>M[V]): M[V]

  /**
    * Unwrap a V value from inside the given m
    *
    * @param m an M[V]
    * @tparam V the value type
    * @return the unwrapped V value
    */
  def get[V](m: M[V]): V

  /**
    * Just for convenience, here is map defined in terms of flatMap and unit
    *
    * @param m the M[V] to be mapped
    * @param f the mapping function, which is a V=>V
    * @tparam V the value type
    * @return an M[V]
    */
  def map[V](m: M[V], f: V=>V): M[V] = flatMap(m, v => unit(f(v)))
}

class FutureWrapper(implicit ec: ExecutionContext, atMost: Duration) extends Wrapper[Future] {
  def flatMap[V](m: Future[V], f: V=>Future[V]): Future[V] = m flatMap f

  override def unit[V](v: =>V): Future[V] = Future.successful(v)

  override def get[V](m: Future[V]): V = Await.result(m,atMost)
}

object Wrapper {
  val optionWrapper = new Wrapper[Option] {
    def flatMap[V](m: Option[V], f: V=>Option[V]): Option[V] = m flatMap f

    override def unit[V](v: =>V): Option[V] = Some(v)

    override def get[V](m: Option[V]): V = m.get
  }
  val tryWrapper = new Wrapper[Try] {
    def flatMap[V](m: Try[V], f: V=>Try[V]): Try[V] = m flatMap f

    override def unit[V](v: =>V): Try[V] = Success(v)

    override def get[V](m: Try[V]): V = m.get
  }
  // XXX The following may look unnecessary -- and will be removed by Organize Imports. But we do need it!
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  implicit val atMost: FiniteDuration = 1.second
  val futureWrapper = new FutureWrapper()
}
abstract class CacheMap[K,V, X[_]](val wrapper: Wrapper[X]) extends mutable.HashMap[K,V] { self =>

  /**
    * This method, analagous to getOrElseUpdate returns the value (wrapped in an "X" container) corresponding to key k if it exists.
    * If it doesn't exist, it invokes the function f to get the value.
    *
    * @param k the key which references the value we need
    * @param f the function which is called to fulfill the value if it doesn't already exist
    * @return an X[V]
    */
  def getOrElseUpdateX(k: K, f: K => X[V]): X[V] = {
    def doUpdate(v: V): X[V] = { self.put(k,v); wrapper.unit(v) }
    get(k) match {
      case Some(v) => wrapper.unit(v)
      case None => wrapper.flatMap[V](f(k), doUpdate _)
    }
  }
}

/**
  * This class is a concrete subclass of CacheMap and uses Option as the container "X".
  *
  * @tparam K they key type
  * @tparam V the value type
  */
class OptionHashMap[K,V] extends CacheMap[K,V,Option](Wrapper.optionWrapper)

/**
  * This class is a concrete subclass of CacheMap and uses Try as the container "X".
  *
  * @tparam K they key type
  * @tparam V the value type
  */
class TryHashMap[K,V] extends CacheMap[K,V,Try](Wrapper.tryWrapper)

/**
  * This class is a concrete subclass of CacheMap and uses Future as the container "X".
  *
  * @tparam K they key type
  * @tparam V the value type
  */
class FutureHashMap[K,V] extends CacheMap[K,V,Future](Wrapper.futureWrapper)
