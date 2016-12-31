package com.phasmid.laScala.values

/**
  * Appendix to Rational class: only for Scala 2.11
  *
  * @author scalaprof
  */
object Rational_Cross {

  def isExactDouble(r: Rational): Boolean = r.toBigDecimal.isExactDouble

}