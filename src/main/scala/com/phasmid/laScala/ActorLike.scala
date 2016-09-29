package com.phasmid.laScala

import scala.concurrent.Future

/**
  * The purpose of this trait is to capture the behavior of a class which delegates to an actor (façade or proxy pattern).
  * This is required, for example, when plugging in a technology that behaves as an actor, while wishing
  * to preserve the option of depending on the technology directly (via a façade).
  *
  * Created by scalaprof on 9/23/16.
  *
  * @tparam T the underlying type
  */
trait ActorLike[T] extends Agent[T] with Queryable[T] with AutoCloseable

/**
  * This trait behaves as a partial function that takes a T as input and returns Unit (i.e nothing).
  * Thus it is (almost) the exact equivalent of Akka's Actor.Receive type.
  *
  * @tparam T the underlying type
  */
trait Agent[T] extends PartialFunction[T,Unit]

/**
  * This trait behaves exactly as the query method does in an actor but takes a query which is a T
  *
  * @tparam T the underlying type
  */
trait Queryable[T] {
  /**
    * Method to accept a query in the form of a T and return an Any response wrapped in Future
    *
    * @param query
    */
  def query(query: T): Future[Any]
}
