package com.phasmid.laScala.fp

import com.phasmid.laScala.Factorial
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class BenchmarkSpec extends FlatSpec with Matchers {
  "Benchmark" should "yield correct number of nanoseconds" in {
    import Benchmark._
    val warmup = 10000.times(Factorial.factorial(40))
//    val start = System.currentTimeMillis()
    val nanos = 10000.times(Factorial.factorial(40))
//    val end = System.currentTimeMillis()
    // NOTE: this might need to be varied according to the speed of the machine, etc.
    nanos.toDouble shouldBe 10.0+-2
  }
}
