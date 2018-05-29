package com.phasmid.laScala.fp

import com.phasmid.laScala.Factorial
import org.scalatest.tagobjects.Slow
import org.scalatest.{FlatSpec, Matchers}

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class BenchmarkSpec extends FlatSpec with Matchers {
  "Benchmark--don't worry if this fails tests under debug or coverage" should "yield correct number of nanoseconds" taggedAs Slow in {
    import Benchmark._
    val benchmark = 10000.times(Factorial.factorial(40))
    println(s"average time for 40! (${benchmark()}) is ${benchmark.nanos/1000} microsecs")
    // NOTE: this might need to be varied according to the speed of the machine, etc.
    benchmark.nanos shouldBe 12000.0 +- 8000
  }
}
