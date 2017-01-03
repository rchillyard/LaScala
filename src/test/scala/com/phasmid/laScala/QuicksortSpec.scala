package com.phasmid.laScala

import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * @author scalaprof
  */
class QuicksortSpec extends FlatSpec with Matchers {
  behavior of "qsort"
  it should "yield appropriate values and type" in {
    val l = List(1, -10, 12, 2)
    val cf = (a: Int, b:Int) => if (a == b) 0 else if (a > b) 1 else -1
    val lSorted = QuickSort.qsort(l)(cf)
    lSorted should matchPattern { case _: List[Int] => }
    lSorted(0) shouldBe -10
    lSorted(1) shouldBe 1
    lSorted(2) shouldBe 2
    lSorted(3) shouldBe 12
    QuickSort.qsort(l.toVector)(cf) should matchPattern { case _: Vector[Int] => }
  }
}
