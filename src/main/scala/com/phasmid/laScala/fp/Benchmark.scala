package com.phasmid.laScala.fp

import com.phasmid.laScala.Factorial

import scala.language.implicitConversions

trait Benchmark[T] extends (()=>T) {
  def nanos: Double
}

/**
  * Declaration of implicit class Rep within Benchmark.
  *
  * Created by scalaprof on 8/17/16.
  */
object Benchmark {

  implicit class Rep(n: Int) {
    /**
      * Method which can be invoked, provided that Benchmark._ has been imported.
      * See for example BenchmarkSpec
      *
      * @param f the function to be invoked
      * @tparam A the result type of f
      * @return the average number of nano-seconds per run
      */
    def times[A](f: => A): Benchmark[A] = {
      // Warmup phase: do at least 20% of repetitions before starting the clock
      1 to (1+n/5) foreach (_ => f)
      val start = System.nanoTime()
      1 to n foreach (_ => f)
      val time = (System.nanoTime() - start) / n.toDouble
      new Benchmark[A] {
        def apply(): A = f

        def nanos: Double = time
      }
    }
  }
}
