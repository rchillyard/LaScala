package com.phasmid.laScala.values

// We really do need the following: import com.phasmid.laScala.values.Rational.RationalHelper
import com.phasmid.laScala.values.Rational.RationalHelper
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class RationalSpec extends FlatSpec with Matchers {

  import com.phasmid.laScala.values.FiniteIntegral.LongIsFiniteIntegral

  "0" should "be OK" in {
    Rational(0) should not be null
  }
  it should "use implicit conversion" in {
    val r: Rational[Long] = 0
    r shouldBe Rational.zero
  }
  it should "be zero" in {
    Rational(0) shouldBe Rational.zero
  }
  it should "be whole" in {
    Rational.zero shouldBe 'whole
  }
  it should "equal 0" in {
    Rational.zero.toInt should be(0)
  }
  it should "equal infinity when inverted" in {
    Rational.zero.invert shouldBe 'infinity
  }
  it should "equal BigDecimal.zero" in {
    Rational.zero.toBigDecimal shouldBe BigDecimal(0)
  }
  it should "equal r when added to r" in {
    val r = Rational[Long](22, 7) // we could choose anything here
    (Rational.zero + r) should be(r)
  }
  it should "equal infinity when r-interpolator has 0 denominator" in {
    r"1/0" shouldBe 'infinity
  }

  "1/2" should "be OK" in {
    Rational.half * 2 shouldBe Rational.one
  }
  it should "equal half" in {
    Rational("1/2") shouldBe Rational.half
  }
  it should "be half of one" in {
    Rational.half * 2 shouldBe Rational.one
  }
  it should "be OK using r-interpolator" in {
    r"1/2" * 2 shouldBe Rational.one
  }
  it should "be OK using r-interpolator with variable" in {
    val denominator = 2
    r"1/$denominator" * denominator shouldBe Rational.one
  }
  it should "yield 0 for floor" in {
    Rational.half.floor shouldBe 0L
  }

  "1" should "be OK" in {
    Rational(1)
  }
  it should "be one" in {
    Rational(1) shouldBe Rational.one
  }
  it should "be one as a String" in {
    Rational("1") shouldBe Rational.one
  }
  it should "be positive" in {
    Rational.one.signum shouldBe 1
  }
  it should "be whole" in {
    Rational.one shouldBe 'whole
  }
  it should "be unity" in {
    Rational.one shouldBe 'unity
  }
  it should "equal 1" in {
    Rational.one.toInt should be(1)
  }
  it should "not equal infinity when inverted" in {
    Rational.one.invert should not be 'infinity
  }
  it should "equal itself when inverted" in {
    Rational.one.invert should be(Rational.one)
  }
  it should "equal BigDecimal.one" in {
    Rational.one.toBigDecimal shouldBe BigDecimal(1)
  }
  it should "equal r when multiplied by r" in {
    val r = Rational[Long](22, 7) // we could choose anything here
    (Rational.one * r) should be(r)
  }
  it should "be -1 when negated" in {
    val r = Rational.one
    -r shouldBe (Rational.one * -1)
    r.signum shouldBe 1
  }

  "-2147483648" should "hash alright" in {
    val r = Rational(-2147483648)
    r.hashCode() shouldBe -2147483618
  }
  it should "be equal to itself" in {
    val r = Rational(-2147483648)
    r shouldEqual r
  }
  "-2147483648/-1" should "hash alright" in {
    val r = Rational[BigInt](-2147483648,-1)
    r.hashCode() shouldBe -2147483617
  }
  it should "be equal to itself" in {
    val r = Rational(-2147483648)
    r shouldEqual r
  }
  "2147483647" should "hash alright" in {
    val r = Rational(2147483647)
    r.hashCode() shouldBe -2147483618
  }
  it should "be equal to itself" in {
    val r = Rational(2147483647)
    r shouldEqual r
  }
  // TODO making this a BigInt rational is cheating really
  "D2147483647" should "hash alright" in {
    val r = Rational[BigInt](1, 2147483647)
    r.hashCode() shouldBe 2147483618
  }
  // TODO making this a BigInt rational is cheating really
  it should "be equal to itself" in {
    val r = Rational[BigInt](1, 2147483647)
    r shouldEqual r
  }

  "power" should "work" in {
    val ten = Rational.ten
    ten.power(2) should equal(Rational(100))
    import FiniteIntegral.LongIsFiniteIntegral
    ten.power(10) should equal(Rational(10000000000L))
  }

  "10" should "be OK" in {
    Rational(10)
  }
  it should "be ten" in {
    Rational(10) shouldBe Rational.ten
  }
  it should "be whole" in {
    Rational.ten shouldBe 'whole
  }
  it should "not be zero" in {
    Rational.ten should not be 'zero
  }
  it should "equal 10" in {
    Rational.ten.toInt should be(10)
  }
  it should "equal 5*2" in {
    (Rational.ten / 2) should be(Rational(5))
  }
  it should "equal 10*1" in {
    (Rational.ten / 10) should be(Rational.one)
  }
  it should "equal BigDecimal(10)" in {
    Rational.ten.toBigDecimal shouldBe BigDecimal(10)
  }
  it should "equal a million when raised to 6th power" in {
    (Rational.ten ^ 6) should be(Rational(1000000))
  }
  it should "barf when raised to 10th power" in {
    val thrown = the[FiniteIntegralException] thrownBy Rational.ten.power(10).toInt
    thrown.getMessage should equal("10000000000 is out of range for class scala.Int$")
  }

  "2/3" should "be OK" in {
    Rational(2, 3)
  }
  it should "equal -1/3 when added to -1" in {
    Rational(2, 3) - Rational.one should be(Rational(-1, 3))
  }
  it should "be less than 1" in {
    Rational(2, 3).compare(Rational.one) should be(-1)
  }
  it should "not be whole" in {
    Rational(2, 3) should not be 'whole
  }
  it should "equal 2 when multiplied by 3" in {
    (Rational(2, 3) * 3 toInt) should be(2)
  }
  it should "equal 3/2 when inverted" in {
    Rational(2, 3).invert should be(Rational(3, 2))
  }
  it should "equal 5/3 when added to 1" in {
    (Rational.one + Rational(2, 3)) should be(Rational(5, 3))
  }
  it should "equal 4/9 when multiplied by itself" in {
    val r = Rational(2, 3)
    (r * r) should be(Rational(4, 9))
  }
  it should "equal 4/9 when squared" in {
    (Rational(2, 3) ^ 2) should be(Rational(4, 9))
  }
  // XXX: this fails with 2.10
  //  it should "barf when toInt invoked" in {
  //    an[RationalException] should be thrownBy Rational(2, 3).toInt
  //    val thrown = the[Exception] thrownBy Rational(2, 3).toInt
  //    thrown.getMessage should equal("2/3 is not Whole")
  //  }

  "2/4" should "not be OK" in {
    val thrown = the[IllegalArgumentException] thrownBy new Rational(2, 4)
    thrown.getMessage should equal("requirement failed: Rational(2,4): arguments have common factor: 2")
  }
  it should "be OK via normalize" in {
    Rational.normalize(2, 4)
  }

  "Floating Point Problem" should "be OK" in {
    val x = Rational(1, 10) + Rational.normalize(2, 10)
    val y = x * 10 / 3
    y shouldBe 'unity
  }

  "BigDecimal" should "convert to Rational" in {
    val pi = BigDecimal(math.Pi)
    val r = Rational(pi)
    r.toDouble should be(math.Pi)
  }

  it should "have a floor of 3" in {
    Rational(BigDecimal(math.Pi)).floor shouldBe 3
  }

  "toString" should "be decimal when not exact: pi" in {
    val pi = Rational(BigDecimal(math.Pi))
    pi.toString() should be("3.141592653589793")
  }

  "Rational(String)" should "work for 0.1" in {
    val r = Rational("0.1")
    r should be(Rational(1, 10))
  }
  it should "work for 1.0e6" in {
    Rational("1.0e6") should be(Rational(10).power(6))
    Rational("1.0E6") should be(Rational(10).power(6))
    Rational("-1.0E6") should be(Rational(10).power(6).negate)
  }
  "sorting" should "work" in {
    val r = List(Rational(1, 2), Rational(2, 3), Rational(1, 3))
    val x = r.sorted
    x.head shouldBe Rational(1, 3)
    x.tail.head shouldBe Rational(1, 2)
    x.tail.tail.head shouldBe Rational(2, 3)
  }

  "r-interpolator" should "work for 1/-2147483648 and -1/-2147483648" in {
    r"1/-2147483648".signum shouldBe -1
    r"-1/-2147483648".signum shouldBe 1
    r"2147483647/0" shouldBe Rational.infinity
  }

  behavior of "Rational as Fractional"

  val f: Fractional[Rational[Long]] = implicitly[Fractional[Rational[Long]]]

  it should "support zero" in {
    f.zero shouldBe Rational.zero
  }
  it should "support one" in {
    f.one shouldBe Rational.one
  }
  it should "support fromInt" in {
    f.fromInt(0) shouldBe Rational.zero
    f.fromInt(1) shouldBe Rational.one
    f.fromInt(-1) shouldBe Rational.one.negate
  }
  it should "support plus" in {
    f.plus(Rational.one, Rational.one) shouldBe Rational(2)
    f.plus(Rational.zero, Rational.one) shouldBe Rational.one
    f.plus(Rational.zero, Rational.zero) shouldBe Rational.zero
  }
  it should "support times" in {
    f.times(Rational.one, Rational.one) shouldBe Rational.one
    f.times(Rational.one, Rational.zero) shouldBe Rational.zero
    f.times(Rational.zero, Rational.zero) shouldBe Rational.zero
    //    an [RationalException] should be thrownBy f.times(Rational(Long.MaxValue),2)
  }
  it should "support div" in {
    f.div(Rational.one, Rational.one) shouldBe Rational.one
    f.div(Rational.one, Rational.zero) shouldBe Rational.infinity
    f.div(Rational.zero, Rational.one) shouldBe Rational.zero
    f.div(Rational.zero, Rational.zero).isNaN shouldBe true
  }
  it should "support compare" in {
    f.compare(Rational.one, Rational.one) shouldBe 0
    f.compare(Rational.one, Rational.zero) shouldBe 1
    f.compare(Rational.zero, Rational.one) shouldBe -1
    f.compare(Rational.zero, Rational.zero) shouldBe 0
  }
  it should "support toLong" in {
    f.toLong(Rational.one) shouldBe 1L
    val half: Rational[Long] = Rational.half
    an[RationalException] should be thrownBy f.toLong(half)
  }
  it should "support toInt" in {
    f.toInt(Rational.one) shouldBe 1L
    an[RationalException] should be thrownBy f.toInt(Rational.half)
    an[FiniteIntegralException] should be thrownBy f.toInt(Rational(Long.MaxValue))
  }
}