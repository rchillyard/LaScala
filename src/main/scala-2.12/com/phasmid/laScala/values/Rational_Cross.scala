package com.phasmid.laScala.values

/**
  * Appendix to Rational class: only for Scala 2.11
  *
  * @author scalaprof
  */
object Rational_Cross {

  def isExactDouble[N: FiniteIntegral](r: Rational[N]): Boolean = r.toBigDecimal.isExactDouble

}