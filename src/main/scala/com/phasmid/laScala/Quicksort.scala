package com.phasmid.laScala

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * With appreciation to evan058 at StackOverflow
  * See http://stackoverflow.com/questions/41420645/scala-make-function-polymorphic-in-data-structure-type
  *
  * NOTE: this is an example of using CanBuildFrom, although in this case it simply sets up an implicit which
  * is picked up by the ++ operator
  *
  * Created by scalaprof on 1/1/17.
  */
object Quicksort {
  def qsort[E, D[E] <: Seq[E]]
  (s: D[E])(c: (E, E) => Int)
  (implicit cbf: CanBuildFrom[D[E], E, D[E]]): D[E] = {
    if (s.size <= 1)
      s
    else {
      val pivot = s(s.size / 2)
      // XXX Don't know why this shows a compiler error. There is no error.
      (qsort(s.filter((x: E) => c(x, pivot) < 0))(c) ++ s.filter((x: E) => c(x, pivot) == 0) ++ qsort(s.filter((x: E) => c(x, pivot) > 0))(c)).to[D]
    }
  }
}