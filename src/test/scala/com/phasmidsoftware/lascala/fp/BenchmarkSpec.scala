package com.phasmidsoftware.lascala.fp

import com.phasmidsoftware.lascala.Factorial
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class BenchmarkSpec extends AnyFlatSpec with Matchers {
  "Benchmark--don't worry if this fails tests under debug or coverage" should "yield correct number of nanoseconds" taggedAs(Slow) in {
    import Benchmark._
    val benchmark = 10000.times(Factorial.factorial(40))
    println(s"average time for 40! (${benchmark()}) is ${benchmark.nanos / 1000} microsecs")
    // NOTE: the time depends on the speed of the machine, so we check only that it is plausible (between 0 and 100 microsecs)
    benchmark.nanos should (be > 0.0 and be < 100000.0)
  }
}
