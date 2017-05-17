package com.phasmid.laScala.fp

import scala.language.implicitConversions

/**
  * FP implementation of a logger.
  *
  * Created by scalaprof on 8/17/16.
  */
object Benchmark {

  implicit class Rep(n: Int) {
    def times[A](f: => A): Long = {val start = System.currentTimeMillis(); 1 to n foreach (_ => f); (System.currentTimeMillis() - start) * 1000 / n}
  }

}
