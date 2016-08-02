package com.phasmid.laScala

import java.time.LocalDate

import com.phasmid.laScala.Incrementable.IncrementableLocalDate
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.Success

/**
  * @author scalaprof
  */
class IncrementableSpec extends FlatSpec with Matchers with Inside {

  "1" should "increment by one" in {
    import com.phasmid.laScala.Incrementable.IncrementableInt
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
    val incrementable = implicitly[Incrementable[LocalDate]]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d)) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 2)
    }
  }
  it should "increment by two days" in {
    implicit val pattern = ""
    val incrementable = implicitly[Incrementable[LocalDate]]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, 2)) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 3)
    }
  }
  it should "increment by one week" in {
    implicit val pattern = ""
    val incrementable = implicitly[Incrementable[LocalDate]]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, by = "w")) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 8)
    }
  }
  it should "increment by two months" in {
    implicit val pattern = ""
    val incrementable = implicitly[Incrementable[LocalDate]]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, 2, "m")) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 3, 1)
    }
  }
  it should "decrement by one year" in {
    implicit val pattern = ""
    val incrementable = implicitly[Incrementable[LocalDate]]
    val dt = for (d <- incrementable.fromString("2016-01-01"); x <- incrementable.increment(d, -1, "y")) yield x
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2015, 1, 1)
    }
  }
}
