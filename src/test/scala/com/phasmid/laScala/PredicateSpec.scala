package com.phasmid.laScala

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author scalaprof
  */
class PredicateSpec extends FlatSpec with Matchers {
  "GT" should "be false for 2,3 and for 3,3" in {
    val p = GT(3)
    p(2) shouldBe false
    p(3) shouldBe false
  }
  it should "be named ok" in {
    GT(3).toString shouldBe ">3"
  }
  "LT" should "be true for 2,3 and for 3,3" in {
    val p = LT(3)
    p(2) shouldBe true
    p(3) shouldBe false
  }
  it should "be named ok" in {
    LT(3).toString shouldBe "<3"
  }
  "GE" should "be false for 2,3 and true for 3,3" in {
    val p = GE(3)
    p(2) shouldBe false
    p(3) shouldBe true
  }
  it should "be named ok" in {
    GE(3).toString shouldBe ">=3"
  }
  "LE" should "be true for 2,3 and true for 3,3" in {
    val p = LE(3)
    p(2) shouldBe true
    p(3) shouldBe true
  }
  it should "be named ok" in {
    LE(3).toString shouldBe "<=3"
  }
  "EQ" should "be true for 2,2 and false for 3,2" in {
    val p = EQ(2)
    p(2) shouldBe true
    p(3) shouldBe false
  }
  it should "be named ok" in {
    EQ(3).toString shouldBe "=3"
  }
  "NE" should "be false for 2,2 and true for 3,2" in {
    val p = NE(2)
    p(2) shouldBe false
    p(3) shouldBe true
  }
  it should "be named ok" in {
    NE(3).toString shouldBe "!=3"
  }
  "map" should "work with toInt" in {
    val p: Predicate[Int] = GT(3)
    val q: Predicate[String] = p transform {_.toInt}
    q.apply("2") shouldBe false
  }
  it should "be named ok" in {
    val p: Predicate[Int] = GT(3)
    val q: Predicate[String] = p transform {_.toInt}
    q.toString shouldBe ">3 transformed by <function1>"
  }
  "it" should "work with a Map" in {
    val p: Predicate[Int] = GT(3)
    val variables: Map[String, Int] = Map("x"->2, "y"->4)
    val q: Predicate[String] = p transform (variables(_))
    q.apply("x") shouldBe false
    q.apply("y") shouldBe true
  }
  "In" should "work" in {
    val values = Seq(1,2,3,5,8,13)
    val p = In(values)
    p(5) shouldBe true
    p(4) shouldBe false
  }
  it should "be named ok" in {
    In(Seq(1,2,3,5,8,13)).toString shouldBe "in (1, 2, 3, 5, 8, 13)..."
  }
  "InRange" should "work" in {
    val p = InRange(1 to 10)
    p(5) shouldBe true
    p(11) shouldBe false
  }
  it should "be named ok" in {
    InRange(1 to 10).toString shouldBe "in Range(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"
  }
  "InBounds" should "work" in {
    val p = InBounds(1, 10)
    p(5) shouldBe true
    p(11) shouldBe false
  }
  it should "be named ok" in {
    InBounds(1, 10).toString shouldBe "in bounds 1..10"
  }
  "Func" should "work" in {
    def even(x: Int) = x%2==0
    val p = Func(even _)
    p(6) shouldBe true
    p(11) shouldBe false
  }
  it should "be named ok" in {
    def even(x: Int) = x%2==0
    val p = Func(even _)
    p.toString shouldBe "function <function1>"
  }
  "Pred" should "work" in {
    val p: Func[Int] = Func(_%2==0)
    val q: Predicate[String] = Pred(p){_.toInt}
    q("6") shouldBe true
    q("11") shouldBe false
  }
  it should "be named ok" in {
    val p: Func[Int] = Func(_%2==0)
    val q: Predicate[String] = Pred(p){_.toInt}
    q.toString shouldBe "function <function1> with <function1>"
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
  it should "be named ok" in {
    val f: Any=>Boolean = {
      case x: Int => x%2==0
      case _ => false
    }
    val p: Func[Any] = Func(f)
    p.toString shouldBe "function <function1>"
  }
  it should "work for regex" in {
    val r = """(\d+)""".r
    val p: Func[String] = Func {
      case r(x) => x.toInt % 2 == 0
      case _ => false
    }
    p("6") shouldBe true
    p("77") shouldBe false
    p("six") shouldBe false
  }
  "Always" should "be true" in {
    val p = Always
    p() shouldBe true
  }
  it should "be named ok" in {
    Always.toString shouldBe "true"
  }
  "Never" should "be false" in {
    val p = Never
    p() shouldBe false
  }
  it should "be named ok" in {
    Never.toString shouldBe "false"
  }
  ":&" should "work with Predicate" in {
    val p = Always :& Never
    p() shouldBe false
  }
  it should "work with function" in {
    val p = Never :| {s: Any =>true}
    p() shouldBe true
  }
  it should "evaluate in the correct order" in {
    val s = new StringBuilder()
    def func1(x: Int): Boolean = {s.append("func1 evaluated. "); x>0}
    def func2(x: Int): Boolean = {s.append("func2 evaluated. "); x<10}
    val p = Func(func1 _) :& Func(func2 _)
    p(3) shouldBe true
    s.toString shouldBe "func1 evaluated. func2 evaluated. "
    s.clear()
    p(-1) shouldBe false
    s.toString shouldBe "func1 evaluated. "
  }
  ":|" should "work with Predicate" in {
    val p = Always :| Never
    p() shouldBe true
  }
  it should "work with function" in {
    val p = Never :| {s: Any =>true}
    p() shouldBe true
  }
  it should "evaluate in the correct order" in {
    val s = new StringBuilder()
    def func1(x: Int): Boolean = {s.append("func1 evaluated. "); x>0}
    def func2(x: Int): Boolean = {s.append("func2 evaluated. "); x<10}
    val p = Func(func1 _) :| func2 _
    p(3) shouldBe true
    s.toString shouldBe "func1 evaluated. "
    s.clear()
    p(-1) shouldBe true
    s.toString shouldBe "func1 evaluated. func2 evaluated. "
  }
  "&:" should "work with Predicate" in {
    val p = Always &: Never
    p() shouldBe false
  }
  it should "work with function" in {
    val p = {s: Any =>true} &: Never
    p() shouldBe false
  }
  it should "evaluate in the correct order" in {
    val s = new StringBuilder()
    def func1(x: Int): Boolean = {s.append("func1 evaluated. "); x>0}
    def func2(x: Int): Boolean = {s.append("func2 evaluated. "); x<10}
    val p = func1 _ &: Func(func2 _)
    p(3) shouldBe true
    s.toString shouldBe "func1 evaluated. func2 evaluated. "
    s.clear()
    p(-1) shouldBe false
    s.toString shouldBe "func1 evaluated. "
  }
  "|:" should "work with Predicate" in {
    val p = Always |: Never
    p() shouldBe true
  }
  it should "work with function" in {
    val p = {s: Any =>true} |: Never
    p() shouldBe true
  }
  it should "evaluate in the correct order" in {
    val s = new StringBuilder()
    def func1(x: Int): Boolean = {s.append("func1 evaluated. "); x>0}
    def func2(x: Int): Boolean = {s.append("func2 evaluated. "); x<10}
    val p = func1 _ |: Func(func2 _)
    p(3) shouldBe true
    s.toString shouldBe "func1 evaluated. "
    s.clear()
    p(-1) shouldBe true
    s.toString shouldBe "func1 evaluated. func2 evaluated. "
  }
}
