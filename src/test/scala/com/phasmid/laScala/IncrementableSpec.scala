package com.phasmid.laScala

import java.time.LocalDate

import com.phasmid.laScala.Incrementable.IncrementableDate
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.{Success, Try}

/**
  * @author scalaprof
  */
class IncrementableSpec extends FlatSpec with Matchers with Inside {

  trait MaybeIncrementable {
    def asIncrementable[X: Incrementable]: Try[X]
  }

  case class MyIncrementable(s: String) extends MaybeIncrementable {
    implicit val pattern = ""

    def asIncrementable[X: Incrementable]: Try[X] = implicitly[Incrementable[X]].fromString(s)

    def increment[X: Incrementable](x: X, y: Int, by: String): Try[X] = implicitly[Incrementable[X]].increment(x, y, by)
  }

  "2016-01-01" should "result in date" in {
    val e = MyIncrementable("2016-01-01")
    val dt: Try[LocalDate] = for (d <- e.asIncrementable; z <- e.increment(d, 1, "")) yield z
    dt should matchPattern { case Success(_) => }
    inside(dt) {
      case Success(d) =>
        d shouldBe LocalDate.of(2016, 1, 2)
    }
  }
}
