package com.phasmid.laScala

import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class RNGSpec extends FlatSpec with Matchers {

  private def stdDev(xs: Seq[Double]): Double = math.sqrt(xs.reduceLeft((a, x) => a + x * x)) / xs.length

  private def mean(xs: Seq[Double]) = xs.sum / xs.length

  def sumU(xs: Seq[random.UniformDouble]): Double = xs.foldLeft(0.0)((a, x) => x + a)

  private def meanU(xs: Seq[random.UniformDouble]) = sumU(xs) / xs.length

  "RNG(0L)" should "match case RNG(-4962768465676381896L)" in {
    val r: RNG[Long] = LongRNG(0L)
    r.next should matchPattern { case LongRNG(-4962768465676381896L) => }
  }
  it should "match case RNG(4804307197456638271L) on next" in {
    val r: RNG[Long] = LongRNG(0L)
    r.next.next should matchPattern { case LongRNG(4804307197456638271L) => }
  }
  "7th element of RNG(0)" should "match case RNG(-4962768465676381896L)" in {
    val lrs = RNG.rngs(LongRNG(0)) take 7 toList;
    (lrs last) should matchPattern { case LongRNG(488730542833106255L) => }
  }
  "Double stream" should "have zero mean" in {
    val xs = RNG.values(DoubleRNG(0)) take 1001 toList;
    math.abs(mean(xs)) shouldBe <=(5E-3)
  }
  "0..1 stream" should "have mean = 0.5" in {
    val xs = RNG.values(UniformDoubleRNG(0)) take 1001 toList;
    math.abs(meanU(xs) - 0.5) shouldBe <=(5E-3)
  }
  "Gaussian stream" should "have zero mean" in {
    val xs = RNG.values2(GaussianRNG(0)) take 11111 toList;
    math.abs(mean(xs)) shouldBe <=(5E-3)
  }
  it should "have unit std. deviation" in {
    val xs = RNG.values2(GaussianRNG(0)) take 11111 toList;
    (math.abs(stdDev(xs)) - 1) shouldBe <=(5E-3)
  }
  "map" should "work" in {
    val rLong: RNG[Long] = LongRNG(-4962768465676381896L)
    val rInt = rLong.map(_.toInt)
    rInt() shouldBe -723955400
    val next = rInt.next
    next() shouldBe 406937919
    val next2 = next.next
    next2() shouldBe 1407270755
  }
  it should "work with map of map" in {
    val rLong: RNG[Long] = LongRNG(0L)
    val rInt = rLong.map(_.toInt)
    val rBoolean = rInt.map(_ % 2 == 0)
    rBoolean() shouldBe true
  }
}