package com.phasmidsoftware.lascala.values

import scala.annotation.nowarn

/**
  * An appendix to Rational, holding what used to differ between Scala versions.
  * CONSIDER folding this back into Rational: the build targets one Scala version now (#142).
  *
  * @author scalaprof
  */
object Rational_Cross {

  @nowarn("msg=unused implicit parameter") // the FiniteIntegral bound stays so that the signature of isExactDouble does not change
  def isExactDouble[N: FiniteIntegral](r: Rational[N]): Boolean = r.toBigDecimal.map(_.isExactDouble) getOrElse false

}