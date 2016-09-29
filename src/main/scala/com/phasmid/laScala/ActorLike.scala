package com.phasmid.laScala

import scala.concurrent.Future

/**
  * The purpose of this trait is to capture the behavior of a class which delegates to an actor (façade or proxy pattern).
  * This is required, for example, when plugging in a technology that behaves as an actor, while wishing
  * to preserve the option of depending on the technology directly (via a façade).
  *
  * Created by scalaprof on 9/23/16.
  */
trait ActorLike extends Agent with Queryable with AutoCloseable

/**
  * This trait behaves as a partial function that takes any input and returns Unit (i.e nothing).
  * Thus it is the exact equivalent of Akka's Actor.Receive type.
  */
trait Agent extends PartialFunction[Any,Unit]

/**
  * This trait behaves exactly as the query method does in an actor.
  */
trait Queryable {
  /**
    * Method to accept a query and return a response wrapped in Future
    *
    * @param query
    */
  def query(query: Any): Future[Any]
}
