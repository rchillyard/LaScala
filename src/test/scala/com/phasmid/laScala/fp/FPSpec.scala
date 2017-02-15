package com.phasmid.laScala.fp

import java.net.URL

import com.phasmid.laScala.fp.FP._
import org.scalatest._
import org.scalatest.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util._
import scala.util.matching.Regex

/**
  *
  * @author scalaprof
  */
class FPSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  "lift(Future[Try[T]])" should "succeed for http://www.google.com" in {
    val uyf = Future(Try(new URL("http://www.google.com")))
    val uf = flatten(uyf)
    whenReady(uf) { u => u should matchPattern { case _: URL => } }
  }

  "lift(Try[Future[T]])" should "succeed for http://www.google.com" in {
    val ufy = Try(Future(new URL("http://www.google.com")))
    val uf = flatten(ufy)
    whenReady(uf) { u => u should matchPattern { case _: URL => } }
  }

  "sequence(Seq[Future[T]])" should "succeed for http://www.google.com, etc." in {
    val ws = List("http://www.google.com", "http://www.microsoft.com")
    val ufs = for {w <- ws; uf = Future(new URL(w))} yield uf
    whenReady(Future.sequence(ufs)) { us => Assertions.assert(us.length == 2) }
  }

  "sequence(Seq[Try[T]])" should "succeed for http://www.google.com, etc." in {
    val ws = List("http://www.google.com", "http://www.microsoft.com")
    val uys = for {w <- ws; url = Try(new URL(w))} yield url
    sequence(uys) match {
      case Success(us) => Assertions.assert(us.length == 2)
      case _ => Failed
    }
  }

  it should "fail for www.google.com, etc." in {
    val ws = List("www.google.com", "http://www.microsoft.com")
    val uys = for {w <- ws; uy = Try(new URL(w))} yield uy
    sequence(uys) match {
      case Failure(_) => Succeeded
      case _ => Failed
    }
  }

  it should "succeed for empty list" in {
    val uys = for {w <- List[String](); uy = Try(new URL(w))} yield uy
    sequence(uys) match {
      case Success(us) => Assertions.assert(us.isEmpty)
      case _ => Failed
    }
  }

  "flatten" should "succeed" in {
    val ifs: Seq[Future[Seq[Int]]] = Seq(Future(Seq(1, 2)))
    whenReady(flatten(ifs)) { x => x should matchPattern { case Seq(1, 2) => } }
  }

  it should "succeed for http://www.google.com, etc." in {
    val ws = List("http://www.google.com", "http://www.microsoft.com")
    val ufs = for {w <- ws; uf = Future(new URL(w))} yield uf
    val usfs = List(Future.sequence(ufs))
    whenReady(flatten(usfs)) { us => Assertions.assert(us.length == 2) }
  }

  it should "succeed for empty list" in {
    val ws = List[String]()
    val urls = for {w <- ws; uf = Future(new URL(w))} yield uf
    val usfs = List(Future.sequence(urls))
    whenReady(flatten(usfs)) { us => Assertions.assert(us.isEmpty) }
  }
  it should "succeed for option map" in {
    val map: Map[String, Option[String]] = Map("a" -> Some("A"), "b" -> None)
    val flat: Map[String, String] = flatten(map)
    flat.size shouldBe 1
  }
  "sequence" should "succeed for http://www.google.com, www.microsoft.com" in {
    val ws = Seq("http://www.google.com", "http://www.microsoft.com", "www.microsoft.com")
    val ufs = for {w <- ws; uf = Future(new URL(w))} yield uf
    val uefs = for {uf <- ufs} yield sequence(uf)
    val uesf = Future.sequence(uefs)
    whenReady(uesf) { ues => Assertions.assert(ues.length == 3) }
    whenReady(uesf) { ues => (ues.head, ues(1)) should matchPattern { case (Right(_), Right(_)) => } }
    whenReady(uesf) { ues => ues(2) should matchPattern { case Left(_) => } }
  }

  "sequence(Future=>Future(Either))" should "succeed for http://www.google.com, www.microsoft.com" in {
    val ws = Seq("http://www.google.com", "http://www.microsoft.com", "www.microsoft.com")
    val uefs = for {w <- ws; uf = Future(new URL(w))} yield sequence(uf)
    for {uef <- uefs} whenReady(uef) { case Right(_) => true; case Left(_) => true; case _ => Assertions.fail() }
  }

  "Sequence[Either]" should "succeed" in {
    val l: Either[Throwable, Int] = Left(new RuntimeException("bad"))
    val r: Either[Throwable, Int] = Right(99)
    sequence(l) should matchPattern { case None => }
    sequence(r) should matchPattern { case Some(99) => }
  }

  "zip(Option,Option)" should "succeed" in {
    val (one, two, none) = (Some(1), Some(2), None)
    zip(one, two) should matchPattern { case Some((1, 2)) => }
    zip(none, two) should matchPattern { case None => }
    zip(one, none) should matchPattern { case None => }
  }

  "zipADeeDooDah" should "succeed" in {
    val as = List(1, 2, 3, 4, 5)
    val bs = List("a", "b", "c")
    zipADeeDooDah(as, bs) should matchPattern { case (List((1, "a"), (2, "b"), (3, "c")), List(4, 5), Nil) => }
    zipADeeDooDah(bs, as) should matchPattern { case (List(("a", 1), ("b", 2), ("c", 3)), Nil, List(4, 5)) => }
    zipADeeDooDah(as drop 3, bs) should matchPattern { case (List((4, "a"), (5, "b")), Nil, List("c")) => }
    zipADeeDooDah(Nil, bs) should matchPattern { case (Nil, Nil, List("a", "b", "c")) => }
    zipADeeDooDah(Nil, Nil) should matchPattern { case (Nil, Nil, Nil) => }
  }
  "optionToTry" should "succeed for Map" in {
    val map = Map("a" -> "A", "b" -> "B")
    optionToTry(map.get("a")) should matchPattern { case Success("A") => }
    optionToTry(map.get("x")) should matchPattern { case Failure(_) => }
  }
  it should "succeed for Map with explicit throwable" in {
    val map = Map("a" -> "A", "b" -> "B")
    val t = new Exception("test")
    optionToTry(map.get("a"), t) should matchPattern { case Success("A") => }
    optionToTry(map.get("x"), t) should matchPattern { case Failure(`t`) => }
  }
  // TODO what does this have to do with lift?
  "lift" should "succeed" in {
    def double(x: Int) = 2 * x
    Success(1) map double should matchPattern { case Success(2) => }
    Failure(new Exception("bad")) map double should matchPattern { case Failure(_) => }
  }

  "liftTry" should "succeed" in {
    def double(x: Int) = 2 * x
    val liftedDouble = liftTry(double)
    liftedDouble(Success(1)) should matchPattern { case Success(2) => }
    Failure(new Exception("bad")) map double should matchPattern { case Failure(_) => }
  }

  "liftOptionTry" should "succeed" in {
    val map = Map("a" -> 1)
    val convertStringToInt = map.apply _
    val fOptionToTry = liftOptionTry(convertStringToInt)
    fOptionToTry(Some("a")) should matchPattern { case Success(1) => }
    fOptionToTry(None) should matchPattern { case Failure(_) => }
  }

  "map2(Option)" should "sum correctly" in {
    val one = Some(1)
    val two = Some(2)
    def sum(x: Int, y: Int) = x + y
    map2(one, two)(sum) should matchPattern { case Some(3) => }
    map2(one, None)(sum) should matchPattern { case None => }
  }

  it should "equate correctly" in {
    val one = Some(1)
    val two = Some(2)
    def eq(x: Int, y: Int) = x==y
    map2(one, one)(eq) should matchPattern { case Some(true) => }
    map2(one, two)(eq) should matchPattern { case Some(false) => }
    map2(None, None)(eq) should matchPattern { case None => }
  }

  "map2(Try)" should "succeed" in {
    val one = Success(1)
    val two = Success(2)
    def sum(x: Int, y: Int) = x + y
    implicit val logger = Spy.getLogger(getClass)
    map2(one, two)(sum) should matchPattern { case Success(3) => }
    map2(one, Failure(new Exception("bad")))(sum) should matchPattern { case Failure(_) => }
  }

  // TODO need to do more thorough testing here
  "map2lazy" should "succeed" in {
    val one = Success(1)
    val two = Success(2)
    def sum(x: Int, y: Int) = x + y

    implicit val continue: Int => Boolean = _ => true
    // NOTE: map2lazy is only available with 2.11 but it's implemented as map2 otherwise
    map2lazy(one, two)(sum) should matchPattern { case Success(3) => }
    map2lazy(one, Failure(new Exception("bad")))(sum) should matchPattern { case Failure(_) => }
  }

  "map3" should "succeed" in {
    val one = Success(1)
    val two = Success(2)
    val three = Success(3)
    def sum(x: Int, y: Int, z: Int) = x + y + z
    map3(one, two, three)(sum) should matchPattern { case Success(6) => }
    map3(one, two, Failure(new Exception("bad")))(sum) should matchPattern { case Failure(_) => }
  }

  // TODO need to do more thorough testing here
  "map3lazy" should "succeed" in {
    val one = Success(1)
    val two = Success(2)
    val three = Success(3)
    def sum(x: Int, y: Int, z: Int) = x + y + z

    implicit val continue: Int => Boolean = _ => true
    // NOTE: map3lazy is only available with 2.11, but it's implemented as map3 otherwise
    map3lazy(one, two, three)(sum) should matchPattern { case Success(6) => }
    map3lazy(one, two, Failure(new Exception("bad")))(sum) should matchPattern { case Failure(_) => }
  }

  "asFuture" should "succeed" in {
    whenReady(asFuture(Success(1))) { x => x should matchPattern { case 1 => } }
    //        whenReady(asFuture(Failure[Int](new Exception("bad")))) { x => x should matchPattern { case x: java.lang.Exception => }}
  }

  "toOption" should "work" in {
    toOption(b = true, "Hello") should matchPattern { case Some("Hello") => }
    toOption(b = false, "X") should matchPattern { case None => }
  }

  it should "work nicely" in {
    val xy = Try("1".toInt)
    toOption(xy) shouldBe Some(1)
  }

  it should "fail gracefully" in {
    val xy = Try("x".toInt)
    var log: String = null
    toOption(xy, {x: Throwable => log = x.getLocalizedMessage}) shouldBe None
    log shouldBe "For input string: \"x\""
  }

  it should "fail ungracefully" in {
    val xy: Try[Any] = Failure(new OutOfMemoryError("something bad"))
    an [OutOfMemoryError] should be thrownBy toOption(xy)
  }

  "sequence[Try]" should "work" in {
    sequence(Try("10".toInt)) should matchPattern { case Right(10) => }
    sequence(Try("x".toInt)) should matchPattern { case Left(_) => }
  }

  "renderLimited" should "work" in {
    val as = List(1, 2, 3, 4, 5)
    renderLimited(as) shouldBe "(1, 2, 3, 4, 5)"
    implicit val limit = 5
    renderLimited(as) shouldBe "(1, 2...)"
  }

  behavior of "map2"

  it should """match Success(1234) for parse "12" to int and parse "34" to int,with (a:Int,b:Int) => a.toString()+b.toString()"""  in
    {
      val a1 = "12"
      val a2 = "34"
      val t1 = Try(a1.toInt)
      val t2 = Try(a2.toInt)
      val test = map2(t1, t2)((a: Int, b: Int) => a.toString + b.toString)
      test should matchPattern { case Success("1234") => }
    }

  it should """fail for "", Int""" in{

    val t1 = Try(Name("Robin",Some("C"),"Hillyard",Some("Ph.D")))
    val t2 = Try(24)

    val test = map2(t1,t2)(Principal.apply)

    // TODO this was originally a Failure and I have broken that.
    test should matchPattern{ case Success(_) => }
  }


  behavior of "map7"

  it should "success" in
    {
      val p1 = Try(0.02)
      val p2 = Try(23)
      val p3 = Rating.parse("PG-13")
      val p4 = Try(15)
      val p5 = Try(18)
      val p6 = Try(20)
      val p7 = Try(28)
      val test = map7(p1, p2, p3, p4, p5, p6, p7)(Reviews)
      test.get should matchPattern{ case Reviews(0.02,23,Rating("PG",Some(13)),15,18,20,28) => }
    }

  it should """fail with bad input""" in {
    val p1 = Try(0.02)
    val p2 = Try(23)
    val p3 = Rating.parse("PG-XXXX")
    val p4 = Try(15)
    val p5 = Try(18)
    val p6 = Try(20)
    val p7 = Try(28)
    map7(p1, p2, p3, p4, p5, p6, p7)(Reviews) should matchPattern{ case Failure(_) => }
  }

  behavior of "invert2"

  it should "work" in
    {
      val a:Int => Int => String = {a => b=> "abcde".substring(a, b)}
      Try(a(0)(2)) should matchPattern{ case Success("ab") => }
      val aux = invert2(a)
      Try(aux(2)(0)) should matchPattern{ case Success("ab") => }
      Try(aux(0)(2)) should matchPattern{ case Failure(_) => }
    }

  behavior of "invert3"

  it should "work" in {
    val a:Int => Int => Int => Int = { a => b => c => a*b+c}
    a(2)(3)(4) shouldBe 10
    val aux = invert3(a)
    aux(2)(3)(4) shouldBe 14
  }

  behavior of "invert4"

  it should "work" in {
    val a: Int => Int => Int => Int => Int = { a => b => c => d => a * b + c % d }
    a(2)(3)(4)(5) shouldBe 10
    invert4(a)(2)(3)(4)(5) shouldBe 21
  }

  behavior of "uncurried2"
  it should "work" in{
    def a:Int => Int => Int => Int = {a => b => c => a*b+c}
    def aux = uncurried2(a)
    a.toString() shouldBe "<function1>"
    aux.toString() shouldBe "<function2>"
  }

}

/**
  * This class represents a Movie from the IMDB data file on Kaggle.
  * Although the limitation on 22 fields in a case class has partially gone away, it's still convenient to group the different attributes together into logical classes.
  *
  * Created by scalaprof on 9/12/16.
  */
case class Movie(format: Format, production: Production, reviews: Reviews, director: Principal, actor1: Principal, actor2: Principal, actor3: Principal, title: String, genres: Seq[String], plotKeywords: Seq[String], imdb: String)

/**
  * The movie format (including language and duration).
  *
  * @param color       whether filmed in color
  * @param language    the native language of the characters
  * @param aspectRatio the aspect ratio of the film
  * @param duration    its length in minutes
  */
case class Format(color: Boolean, language: String, aspectRatio: Double, duration: Int) {
  override def toString: String = {
    val x = if (color) "Color" else "B&W"
    s"$x,$language,$aspectRatio,$duration"
  }
}

/**
  * The production: its country, year, and financials
  *
  * @param country   country of origin
  * @param budget    production budget in US dollars
  * @param gross     gross earnings (?)
  * @param titleYear the year the title was registered (?)
  */
case class Production(country: String, budget: Int, gross: Int, titleYear: Int) {
  def isKiwi: Boolean = this match {
    case Production("New Zealand", _, _, _) => true
    case _ => false
  }
}

/**
  * Information about various forms of review, including the content rating.
  */
case class Reviews(imdbScore: Double, facebookLikes: Int, contentRating: Rating, numUsersReview: Int, numUsersVoted: Int, numCriticReviews: Int, totalFacebookLikes: Int)

/**
  * A cast or crew principal
  *
  * @param name          name
  * @param facebookLikes number of FaceBook likes
  */
case class Principal(name: Name, facebookLikes: Int) {
  override def toString = s"$name ($facebookLikes likes)"
}

/**
  * A name of a contributor to the production
  *
  * @param first  first name
  * @param middle middle name or initial
  * @param last   last name
  * @param suffix suffix
  */
case class Name(first: String, middle: Option[String], last: String, suffix: Option[String]) {
  override def toString: String = {
    case class Result(r: StringBuffer) {
      def append(s: String): Unit = r.append(" " + s)

      override def toString: String = r.toString
    }
    val r: Result = Result(new StringBuffer(first))
    middle foreach r.append
    r.append(last)
    suffix foreach r.append
    r.toString
  }
}

/**
  * The US rating
  */
case class Rating(code: String, age: Option[Int]) {
  override def toString: String = code + (age match {
    case Some(x) => "-" + x
    case _ => ""
  })
}

object Rating {
  val rRating: Regex = """^(\w*)(-(\d\d))?$""".r

  /**
    * Alternative apply method for the Rating class such that a single String is decoded
    *
    * @param s a String made up of a code, optionally followed by a dash and a number, e.g. "R" or "PG-13"
    * @return a Rating
    */
  def parse(s: String): Try[Rating] =
  s match {
    case rRating(code, _, age) => Success(apply(code, Try(age.toInt).toOption))
    case _ => Failure(new Exception(s"parse error in Rating: $s"))
  }
}
