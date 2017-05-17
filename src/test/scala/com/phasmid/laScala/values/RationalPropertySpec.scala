package com.phasmid.laScala.values

/**
  * Created by scalaprof on 10/4/16.
  */

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
  * Created by scalaprof on 10/4/16.
  */
class RationalPropertySpec extends Properties("String") {


  property("RationalFromString") = forAll { (a: Int, b: Int) =>
    import Rational.RationalHelper
    val rat = r"$a/$b"
    b == 0 || (rat * b).toLong == a
  }

}