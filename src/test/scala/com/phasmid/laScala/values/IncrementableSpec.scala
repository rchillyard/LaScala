package com.phasmid.laScala.values

import java.time.LocalDate

import com.phasmid.laScala.values.Incrementable.IncrementableLocalDate
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.Success

/**
  * @author scalaprof
  */
class IncrementableSpec extends FlatSpec with Matchers with Inside {

  behavior of "zero"
  it should "yield 0 for Int" in {
    import Incrementable.IncrementableInt
    IncrementableInt.zero shouldBe 0
  }
  it should "yield 0L for Long" in {
    import Incrementable.IncrementableLong
    IncrementableLong.zero shouldBe 0L
  }
  it should "yield A for String" in {
    import Incrementable.IncrementableString
    IncrementableString.zero shouldBe "A"
  }
  it should "yield now for LocalDate" in {
    import Incrementable.IncrementableLocalDate
    val now = LocalDate.now()
    val zero = IncrementableLocalDate.zero
    zero.isAfter(now) || zero.isEqual(now) shouldBe true
  }

  behavior of "IncrementableString"
  it should "give B after zero" in {
    import Incrementable.IncrementableString
    IncrementableString.increment(IncrementableString.zero) should matchPattern { case Success("B") => }
  }
  it should "give Z after zero and 25 steps" in {
    import Incrementable.IncrementableString
    IncrementableString.increment(IncrementableString.zero, 25) should matchPattern { case Success("Z") => }
  }
  it should "give AA after zero and 26 steps" in {
    import Incrementable.IncrementableString
    IncrementableString.increment(IncrementableString.zero, 26) should matchPattern { case Success("AA") => }
  }
  it should "give ZA after zero and 676 steps" in {
    import Incrementable.IncrementableString
    IncrementableString.increment(IncrementableString.zero, 676) should matchPattern { case Success("ZA") => }
  }
  it should "give AAA after zero and 702 steps" in {
    import Incrementable.IncrementableString
    IncrementableString.increment(IncrementableString.zero, 702) should matchPattern { case Success("AAA") => }
  }

  "1" should "increment by one" in {
    import Incrementable.IncrementableInt
    implicit val pattern = ""
    val xt = for (x <- IncrementableInt.fromString("1"); y <- IncrementableInt.increment(x)) yield y
    xt should matchPattern { case Success(_) => }
    inside(xt) {
      case Success(x) =>
        x shouldBe 2
    }
  }
  "2016-01-01" should "increment by one day" in {
    implicit val pattern = ""
    val incrementable = Incrementable[LocalDate]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d)) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 2)
    }
  }
  it should "increment by two days" in {
    implicit val pattern = ""
    val incrementable = Incrementable[LocalDate]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, 2)) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 3)
    }
  }
  it should "increment by one week" in {
    implicit val pattern = ""
    val incrementable = Incrementable[LocalDate]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, by = "w")) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 8)
    }
  }
  it should "increment by two months" in {
    implicit val pattern = ""
    val incrementable = Incrementable[LocalDate]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, 2, "m")) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 3, 1)
    }
  }
  it should "decrement by one year" in {
    implicit val pattern = ""
    val incrementable = Incrementable[LocalDate]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, -1, "y")) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2015, 1, 1)
    }
  }
}
