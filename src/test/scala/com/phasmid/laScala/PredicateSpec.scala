package com.phasmid.laScala

import org.scalatest.{FlatSpec, Matchers}

import scala.util._

/**
  * @author scalaprof
  */
class PredicateSpec extends FlatSpec with Matchers {
  "GT" should "be false for 2,3 and for 3,3" in {
    val p = GT(3)
    p(2) shouldBe false
    p(3) shouldBe false
  }
  "LT" should "be true for 2,3 and for 3,3" in {
    val p = LT(3)
    p(2) shouldBe true
    p(3) shouldBe false
  }
  "GE" should "be false for 2,3 and true for 3,3" in {
    val p = GE(3)
    p(2) shouldBe false
    p(3) shouldBe true
  }
  "LE" should "be true for 2,3 and true for 3,3" in {
    val p = LE(3)
    p(2) shouldBe true
    p(3) shouldBe true
  }
  "EQ" should "be true for 2,2 and false for 3,2" in {
    val p = EQ(2)
    p(2) shouldBe true
    p(3) shouldBe false
  }
  "NE" should "be false for 2,2 and true for 3,2" in {
    val p = NE(2)
    p(2) shouldBe false
    p(3) shouldBe true
  }
  "map" should "work with toInt" in {
    val p: Predicate[Int] = GT(3)
    val q: Predicate[String] = p map {_.toInt}
    q.apply("2") shouldBe false
  }
  "it" should "work with a Map" in {
    val p: Predicate[Int] = GT(3)
    val variables: Map[String, Int] = Map("x"->2, "y"->4)
    val q: Predicate[String] = p map (variables(_))
    q.apply("x") shouldBe false
    q.apply("y") shouldBe true
  }
  "In" should "work" in {
    val values = Seq(1,2,3,5,8,13)
    val p = In(values)
    p(5) shouldBe true
    p(4) shouldBe false
  }
  "InRange" should "work" in {
    val p = In(1 to 10)
    p(5) shouldBe true
    p(11) shouldBe false
  }
  "Func" should "work" in {
    def even(x: Int) = x%2==0
    val p = Func(even _)
    p(6) shouldBe true
    p(11) shouldBe false
  }
  "Pred" should "work" in {
    val p: Func[Int] = Func(_%2==0)
    val q: Predicate[String] = Pred(p){_.toInt}
    q("6") shouldBe true
    q("11") shouldBe false
  }
  "Matches" should "work for anonymous function" in {
    val f: Any=>Boolean = {
      case x: Int => x%2==0
      case _ => false
    }
    val p: Func[Any] = Func(f)
    p(6) shouldBe true
    p(7) shouldBe false
    p("six") shouldBe false
  }
  it should "work for regex" in {
    val r = """(\d+)""".r
    val p: Func[String] = Func(_ match {
      case r(x) => x.toInt%2==0
      case _ => false
    })
    p("6") shouldBe true
    p("77") shouldBe false
    p("six") shouldBe false
  }
  "Always" should "be true" in {
    val p = Always
    p() shouldBe true
  }
  "Never" should "be false" in {
    val p = Never
    p() shouldBe false
  }
//  ":&" should "work" in {
//    val p = Always :& Never
//    p() shouldBe false
//  }
//  ":|" should "work" in {
//    val p = Always :| Never
//    p() shouldBe true
//  }
}
