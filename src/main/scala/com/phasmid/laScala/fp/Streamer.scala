/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import scala.annotation.tailrec

/**
  * This class is based on a mutable Stream.
  * Its purpose is like a one-time-pad: each value is yielded by the Streamer once and once only.
 *
  * @param s the Stream
  * @tparam X the underlying type and the type of the result
  */
case class Streamer[X](private var s: Stream[X]) extends (()=>X) {

  apply() // We need to skip over the first value

  /**
    * This method mutates this Streamer by resetting the value of s to its tail and returning its head.
    * @return the head of the Stream
    */
  override def apply(): X = s match {
    case x #:: tail => s = tail; x
  }

  def take(n: Int): Seq[X] = {
    @tailrec def inner(xs: Seq[X], i: Int): Seq[X] = if (i==0) xs else inner(xs :+ this(), i-1)
    inner(Seq.empty,n)
  }
}
