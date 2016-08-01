package com.phasmid.laScala

import scala.language.postfixOps
import scala.util.Random

/**
  * Case class for shuffling a sequence.
  *
  * NOTE: we do not use RNG here because Random includes a shuffle method. But we can get our seed from a RNG[Long]
  *
  * @param seed the seed for the random number generator
  * @tparam A the underlying type of the Seq to shuffle
  */
case class Shuffle[A](seed: Long) extends (Seq[A] => Seq[A]) {
  /**
    * The apply method for this function.
    *
    * @param source a Seq which needs to be shuffled
    * @return a Seq with greater entropy than the source.
    */
  def apply(source: Seq[A]): Seq[A] = new Random(seed).shuffle(source)
}
