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

  property("RationalHashMapEquals") = forAll { (a: Int, b: Int) =>
    // NOTE: we use Rational[BigInt] here, even though a and b are Int
    // The problem is that when a = Int.MinValue, we get a problem.
    // It's not entirely obvious how to work around it.
    val r1 = Rational[BigInt](a, b)
    val r2 = Rational[BigInt](a, b)
    if (r1 == r2) r1.hashCode()==r2.hashCode()
    else false
  }

}