/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.fp

import com.phasmid.laScala.{LongRNG, RNG}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 4/7/17.
  */
class StreamerSpec extends FlatSpec with Matchers {
  behavior of "apply"

  it should "work with random number generator" in {
    val lr = LongRNG(-4962768465676381896L)
    val rs = RNG.values(lr)
    rs.head == rs.head shouldBe true
    val target = Streamer(rs)
    target() shouldBe 4804307197456638271L
    target() == target() shouldBe false
  }

  behavior of "take"

  it should "work with random number generator" in {
    val target = Streamer(RNG.values(LongRNG(0L)))
    target.take(4) shouldBe Seq(-4962768465676381896L, 4804307197456638271L, -1034601897293430941L, 7848011421992302230L)
  }

}
