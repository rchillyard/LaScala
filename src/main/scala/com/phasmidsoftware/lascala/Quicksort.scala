package com.phasmidsoftware.lascala

import scala.collection.Factory

/**
  * With appreciation to evan058 at StackOverflow
  * See http://stackoverflow.com/questions/41420645/scala-make-function-polymorphic-in-data-structure-type
  *
  * NOTE: this is an example of using Factory (formerly CanBuildFrom) to rebuild the original collection type
  *
  * Created by scalaprof on 1/1/17.
  */
object Quicksort {
  /**
    * TODO fix the shadowing problem
    */
  //noinspection TypeParameterShadow
  def qsort[E, D[E] <: Seq[E]]
  (s: D[E])(c: (E, E) => Int)
  (implicit f: Factory[E, D[E]]): D[E] = {
    if (s.size <= 1)
      s
    else {
      val pivot = s(s.size / 2)
      (qsort(s.filter((x: E) => c(x, pivot) < 0).to(f))(c) ++ s.filter((x: E) => c(x, pivot) == 0) ++ qsort(s.filter((x: E) => c(x, pivot) > 0).to(f))(c)).to(f)
    }
  }
}