package com.phasmid.laScala

import java.util.Random

/**
  * Very primitive implementation of a state-based random number generator
  *
  * CONSIDER reworking this entire package, particularly using map instead of defining things like DoubleRNG.
  *
  * CONSIDER having RNG extend FilterMonadic or at least implement flatMap
  *
  * @tparam A the underlying type of this Random Number Generator
  */
trait RNG[+A] extends (() => A) {
  def next: RNG[A]

  def map[B](f: A => B): RNG[B]
}

/**
  * An abstract base class which implements some of the basic functionality of RNG
  *
  * @param n the Long value which defines this state of the RNG
  * @tparam A the underlying type of this Random Number Generator
  */
abstract class RNG_Java[+A](n: Long, f: Long => A) extends RNG[A] {
  self =>

  /**
    * The apply method: takes no parameter and yields an A
    *
    * @return A
    */
  def apply: A = f(n)

  /**
    * Method to build a new instance of this RNG
    *
    * @param n the state
    * @return a new instance of RNG as a RNG_Java
    */
  def buildNew(n: Long): RNG_Java[A]

  /**
    * Method to yield the next seed value.
    * This implementation simply calls the nextSeed method in the companion object.
    *
    * may be overridden (if you want to define your own pseudo-random sequence)
    *
    * @return a Long value
    */
  def nextSeed: Long = RNG_Java.nextSeed(n)

  /**
    * Method to yield the next RNG in the pseudo-random sequence.
    * Extenders don't normally need to override this.
    *
    * @return
    */
  def next: RNG_Java[A] = buildNew(nextSeed)

  /**
    * Method to get the state -- private
    *
    * @return
    */
  private[laScala] def state = n

  /**
    * Method to map this RNG into another RNG
    *
    * @param f the map function which should be applied to the underlying value.
    * @tparam B the underlying type of the new RNG
    * @return a new RNG[B]
    */
  def map[B](f: (A) => B): RNG[B] = {
    class RNG_B(n: Long) extends RNG_Java[B](n, self.f andThen f) {
      def buildNew(n: Long): RNG_Java[B] = new RNG_B(n)
    }
    new RNG_B(n)
  }

  override def toString = s"RNG: state=$n; value=${apply()}"
}

object RNG_Java {
  def nextSeed(n: Long): Long = new Random(n).nextLong
}

case class LongRNG(n: Long) extends RNG_Java[Long](n, identity) {
  def buildNew(n: Long) = LongRNG(n)
}

case class DoubleRNG(n: Long) extends RNG_Java[Double](n, { _.toDouble / Long.MaxValue }) {
  def buildNew(n: Long) = DoubleRNG(n)

  override def toString = s"DoubleRNG: $n->$apply"
}

/**
  * This class is a random-number-generator for values uniformly distributed in the range 0..1
  */
case class UniformDoubleRNG(n: Long) extends RNG_Java[UniformDouble](n, { n => UniformDouble(math.abs(n.toDouble / Long.MaxValue), Unit) }) {
  def buildNew(n: Long) = UniformDoubleRNG(n)

  override def toString = s"UniformDoubleRNG: $n->$apply"
}

/**
  * This class is a random-number-generator for values which are normally distributed with mean: 0 and standard deviation: 1
  *
  * See <a href="https://en.wikipedia.org/wiki/Normal_distribution#Generating_values_from_normal_distribution">
  * Generating values from normal distribution (Box-Muller method)
  * </a>
  */
case class GaussianRNG(n: Long) extends RNG_Java[(Double, Double)](n, GaussianRNG.values) {
  def buildNew(n: Long) = GaussianRNG(n)

  override def nextSeed: Long = RNG_Java.nextSeed(GaussianRNG.logic(n)._2.state)

  override def toString = s"GaussianRNG: $n->(${apply._1},${apply._2})"
}

object RNG {
  type SRNG[A] = Stream[RNG[A]]

  def rngs[A](ar: RNG[A]): SRNG[A] = Stream.cons(ar, rngs(ar.next))

  def values[A](ar: RNG[A]): Stream[A] = rngs(ar) map {
    _.apply()
  }

  def values2[A](aAr: RNG[(A, A)]): Stream[A] = rngs(aAr) flatMap { x => Stream(x.apply()._1, x.apply()._2) }
}

object LongRNG {
  def apply: RNG[Long] = LongRNG(System.currentTimeMillis())
}

/**
  * This is essentially a wrapper of Double where (implicitly) 0 <= x <= 1.
  * Note that we would like to specify a require statement but such are not legal in Value types
  */
case class UniformDouble(x: Double) {
  def +(y: Double) = x + y
}

object UniformDoubleRNG {
  def apply: RNG[UniformDouble] = UniformDoubleRNG(System.currentTimeMillis())
}

object GaussianRNG {
  def apply: RNG[(Double, Double)] = GaussianRNG(System.currentTimeMillis())

  def logic(n: Long) = {
    val r1 = UniformDoubleRNG(n)
    val r2 = r1.next
    val u = r1.apply.x
    val v = r2.apply.x // ???
    val k = if (u <= 0) 0 else math.sqrt(-2 * math.log(u))
    val tuple = (k * math.cos(2 * math.Pi * v), k * math.sin(2 * math.Pi * v))
    (tuple, r2)
  }

  def values(n: Long) = logic(n)._1
}

object UniformDouble {
  def apply(x: Double, y: Unit): UniformDouble = if (x >= 0 && x <= 1) new UniformDouble(x) else throw new IllegalArgumentException(s"$x is not in range 0..1")

  def +(x: Double, y: UniformDouble) = y + x
}