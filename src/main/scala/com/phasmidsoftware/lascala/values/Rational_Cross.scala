package com.phasmidsoftware.lascala.values

import scala.annotation.nowarn

/**
  * Appendix to Rational class: only for Scala 2.11
  *
  * @author scalaprof
  */
object Rational_Cross {

  @nowarn("msg=unused implicit parameter") // the FiniteIntegral bound stays so that the signature of isExactDouble does not change
  def isExactDouble[N: FiniteIntegral](r: Rational[N]): Boolean = r.toBigDecimal.map(_.isExactDouble) getOrElse false

}