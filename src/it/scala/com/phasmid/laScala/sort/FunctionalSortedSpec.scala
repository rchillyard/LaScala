/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.sort

import com.phasmid.laScala.fp.Benchmark
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * @author scalaprof
  */
class FunctionalSortedSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  case class Composite(i: Int, s: String)

  object Composite {

    object OrderingCompositeInt extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.i.compare(y.i)
    }

    object OrderingCompositeString extends Ordering[Composite] {
      def compare(x: Composite, y: Composite): Int = x.s.compare(y.s)
    }

  }

  behavior of "Sorted"

  it should "sort asynchronously" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = List(3, 1, 2)
    val sorted = Sorted.create(list)
    val xsf = sorted.async
    whenReady(xsf) { xs => xs shouldBe List(1, 2, 3) }
  }
  it should "sort async" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = Seq.fill(1000)(Random.nextInt()).toList
    val sorted = Sorted.create(list)
    import Benchmark._
    val benchmark = 10.times {
      val xsf = sorted.async
      val bf = xsf.map (Sorted.verify(_))
      whenReady(bf) { xs => xs shouldBe true }
    }
    println(s"async benchmark: $benchmark")
  }
  it should "sort in parallel" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = Seq.fill(1000)(Random.nextInt()).toList
    val sorted = Sorted.create(list)
    import Benchmark._
    val benchmark = 10.times {
      val xsf = sorted.parSort
      val bf = xsf.map (Sorted.verify(_))
      whenReady(bf) { xs => xs shouldBe true }
    }
    println(s"parallel benchmark: $benchmark")
  }

  behavior of "merge"
  it should "work" in {
    val l1 = List(1, 5, 8, 10, 11, 15)
    val l2 = List(3, 4, 9, 12, 14, 16)
    Sorted.merge(l1, l2) shouldBe List(1, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16)
  }
}