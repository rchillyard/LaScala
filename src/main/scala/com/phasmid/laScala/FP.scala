package com.phasmid.laScala

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.util._

/**
  * This is a collection of general functional-programming-style methods.
  * We could probably get these from ScalaZ but I'd rather have no dependencies.
  *
  * TODO flesh out the unit tests (coverage is low).
  *
  * @author scalaprof
  */
object FP {

  def flatten[X](xyf: Future[Try[X]])(implicit executor: ExecutionContext): Future[X] = for (xy <- xyf; x <- asFuture(xy)) yield x

  /**
    * this isn't a particularly useful method: basically, it just strips away the Try part, returning an un-fulfilled Future.
    *
    * @param xfy a Future[X] value wrapped in Try
    * @tparam X the uderlying type
    * @return an X value wrapped in Future
    */
  def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

  /**
    *
    * @param xsfs a sequence of futures, each wrapping a sequence of X
    * @param ec the execution context
    * @tparam X the uderlying type
    * @return a sequence of X values wrapped in Future
    */
  def flatten[X](xsfs: Seq[Future[Seq[X]]])(implicit ec: ExecutionContext): Future[Seq[X]] = Future.sequence(xsfs) map {
    _ flatten
  }

  /**
    * TODO unit test
    *
    * @param esf      the collection to process
    * @param f        the function to process left-hand-sides
    * @param executor the executor to use
    * @tparam X the uderlying type
    * @return a Future\[Seq\[X\]\]
    */
  def flattenRecover[X](esf: Future[Seq[Either[Throwable, Seq[X]]]], f: => Throwable => Unit)(implicit executor: ExecutionContext): Future[Seq[X]] = {
    def filter(uses: Seq[Either[Throwable, Seq[X]]]): Seq[X] = {
      val uses2 = for {use <- uses; if (use match {
        case Left(x) => f(x); false;
        case _ => true
      })} yield use
      val uss = for {use <- uses2; uso = sequence(use); us <- uso} yield us
      uss flatten
    }
    for {es <- esf; e = filter(es)} yield e
  }

  def flatten[K, V](voKm: Map[K, Option[V]]): Map[K, V] = for ((k, vo) <- voKm; v <- vo) yield k -> v

  def asFuture[X](xy: => Try[X]): Future[X] = xy match {
    case Success(s) => Future.successful(s)
    case Failure(e) => Future.failed(e)
  }

  /**
    * TODO unit test
    *
    * @param xy a Try[X] value
    * @tparam X the uderlying type
    * @return Right[X] if successful, else a Left[X]
    */
  def sequence[X](xy: => Try[X]): Either[Throwable, X] =
    xy match {
      case Success(s) => Right(s)
      case Failure(e) => Left(e)
    }

  def sequence[X](xf: Future[X])(implicit executor: ExecutionContext): Future[Either[Throwable, X]] =
    xf transform( { s => Right(s) }, { f => f }) recoverWith { case f => Future(Left(f)) }

  def sequence[X](xfs: Seq[Future[X]])(implicit executor: ExecutionContext): Seq[Future[Either[Throwable, X]]] = for (xf <- xfs) yield sequence(xf)

  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = (Try(Seq[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
    * TODO unit test
    *
    * @param xys a Stream of Try[X] values
    * @tparam X the underlying type
    * @return a Stream[X] wrapped in a Try
    */
  def sequence[X](xys: Stream[Try[X]]): Try[Stream[X]] = (Try(Stream[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
    * TODO unit test
    *
    * @param xos a sequence of Option[X] values
    * @tparam X the underlying type
    * @return a sequence of X values wrapped in Option
    */
  def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = (Option(Seq[X]()) /: xos) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  /**
    * TODO unit test
    * @param xe an Either[Throwable,X] object
    * @tparam X the underlying type
    * @return an X value wrapped inside Option
    */
  def sequence[X](xe: Either[Throwable, X]): Option[X] = xe.right.toOption

  /**
    * TODO unit test
    * zip two options into an option of a tuple
    * @param ao an A wrapped inside Option
    * @param bo a B wrapped inside Option
    * @tparam A the underlying type of the first parameter
    * @tparam B the underlying type of the second parameter
    * @return a tuple containing an A and a B value, all wrapped inside Option
    */
  def zip[A, B](ao: Option[A], bo: Option[B]): Option[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  /**
    * TODO unit test
    * zip two try values into a Try of a tuple
    * @param ao an A wrapped inside Try
    * @param bo a B wrapped inside Try
    * @tparam A the underlying type of the first parameter
    * @tparam B the underlying type of the second parameter
    * @return a tuple containing an A and a B value, all wrapped inside Try
    */
  def zip[A, B](ao: Try[A], bo: Try[B]): Try[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  /**
    * TODO unit test
    * zip two Future values into a Future of a tuple
    * @param ao an A wrapped inside Future
    * @param bo a B wrapped inside Future
    * @tparam A the underlying type of the first parameter
    * @tparam B the underlying type of the second parameter
    * @return a tuple containing an A and a B value, all wrapped inside Future
    */
  def zip[A, B](ao: Future[A], bo: Future[B])(implicit ec: ExecutionContext): Future[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  /**
    * TODO unit test
    *
    * @param xo an X value wrapped in Option
    * @param t a throwable which will be returned if xo is None
    * @tparam X the underlying type of the input and the output
    * @return an X value wrapped as a Try
    */
  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = Try(xo.get).recoverWith { case e: java.util.NoSuchElementException => Failure(t) }

  /**
    * TODO unit test
    *
    * @param xo an X value wrapped in Option
    * @tparam X the underlying type of the input and the output
    * @return an X value wrapped as a Try. If xo is None, then the result will be a NoSuchElementException
    */
  def optionToTry[X](xo: Option[X]): Try[X] = Try(xo.get)

  /**
    * TODO unit test
    *
    * method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
    * @param ty1 a Try[T] value
    * @param ty2 a Try[T] value
    * @param f function which takes two T parameters and yields a U result
    * @tparam T the input type
    * @tparam U the result type
    * @return a Try[U]
    */
  def map2[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2} yield f(t1, t2)

  /**
    * TODO unit test
    *
    * method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
    * @param ty1 a Try[T] value
    * @param ty2 a Try[T] value passed as call-by-name
    * @param f function which takes two T parameters and yields a U result
    * @param g (implicit) guard function which, given the first parameter's value, must be true for the second parameter (ty2) to be evaluated
    * @param default (implicit) a default value
    * @tparam T the input type
    * @tparam U the result type
    * @return a Try[U]
    */
  def map2lazy[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U)(implicit g: T => Boolean = { x: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    (for {t1 <- ty1; if g(t1); t2 <- ty2} yield f(t1, t2)) recoverWith { case z: java.util.NoSuchElementException => default }

  /**
    * TODO unit test
    *
    * Similar to map2
    *
    * @param ty1 a Try[T] value
    * @param ty2 a Try[T] value
    * @param ty3 a Try[T] value
    * @param f function which takes three T parameters and yields a U result
    * @tparam T the input type
    * @tparam U the result type
    * @return a Try[U]
    */
  def map3[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2; t3 <- ty3} yield f(t1, t2, t3)

  /**
    * TODO unit test
    *
    * method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
    * @param ty1 a Try[T] value
    * @param ty2 a Try[T] value passed as call-by-name
    * @param ty3 a Try[T] value passed as call-by-name
    * @param f function which takes two T parameters and yields a U result
    * @param g (implicit) guard function which, given the first parameter's value, must be true for the second parameter (ty2) to be evaluated;
    *          and which, given the second parameter's value, must be true for the third parameter (ty3) to be evaluated
    * @param default (implicit) a default value
    * @tparam T the input type
    * @tparam U the result type
    * @return a Try[U]
    */
  def map3lazy[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U)(implicit g: T => Boolean = { x: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    (for {t1 <- ty1; if g(t1); t2 <- ty2; if g(t2); t3 <- ty3} yield f(t1, t2, t3)) recoverWith { case z: java.util.NoSuchElementException => default }

  /**
    * TODO unit test
    *
    * @param f a function which transforms an X into a Y
    * @param xt an X value wrapped as a Try[X]
    * @tparam X the input type of f
    * @tparam Y the output type of f
    * @return a Try[Y]
    */
  def lift[X, Y](f: X => Y)(xt: Try[X]): Try[Y] = xt map f

  /**
    * TODO unit test
    *
    * @param f a function which transforms an X into a Y
    * @param x an X value
    * @tparam X the input type of f
    * @tparam Y the output type of f
    * @return a Try[Y]
    */
  def trial[X, Y](f: X => Y)(x: => X): Try[Y] = lift(f)(Try(x))

  /**
    * TODO unit test
    *
    * A true "lift" method which takes a function f (T=>U) and returns a Try[T]=>Try[T]
    * @param f a function which transforms an T into a U
    * @tparam T the T type
    * @tparam U the U type
    * @return the corresponding function which takes a Try[T] and returns a Try[U]
    */
  def liftTry[T, U](f: T => U): Try[T] => Try[U] = _ map f

  /**
    * TODO unit test
    *
    * A pure lift method to lift a function into Try
    *
    * @param f a function which takes two T parameters and returns a U
    * @tparam T the uderlying source type
    * @tparam U the underlying result type
    * @return the corresponding function which takes to Try[T] parameters and returns a Try[U]
    */
  def lift2Try[T, U](f: (T, T) => U): (Try[T], Try[T]) => Try[U] = map2(_, _)(f)

  /**
    * TODO unit test
    *
    * A pure lift method to lift a function into Option
    *
    * @param f a function which takes two T parameters and returns a U
    * @tparam T the uderlying source type
    * @tparam U the underlying result type
    * @return the corresponding function which takes to Option[T] parameters and returns an Option[U]
    */
  def liftOption[T, U](f: T => U): Option[T] => Option[U] = _ map f

  /**
    * TODO unit test
    *
    * A pure lift method to lift a function into Future
    *
    * @param f a function which takes two T parameters and returns a U
    * @param executor (implicit) execution context
    * @tparam T the uderlying source type
    * @tparam U the underlying result type
    * @return the corresponding function which takes to Future[T] parameters and returns a Future[U]
    */
  def liftFuture[T, U](f: T => U)(implicit executor: ExecutionContext): Future[T] => Future[U] = _ map f

  implicit val limit = 25

  /**
    * TODO unit test
    *
    * @param as a sequence of As
    * @param limit the limit of how many you want to show
    * @tparam A the underlying type of the sequence
    * @return a String representing the first "limit" elements of as
    */
  def renderLimited[A](as: => Seq[A])(implicit limit: Int = as.length*5): String = {
    val iter = as.toStream.toIterator
    val buffer = new StringBuilder("(")
    while (iter.hasNext && buffer.length < limit) {
      if (buffer.length > 1) buffer append ", "
      buffer append s"${iter.next}"
    }
    if (iter.hasNext) buffer append "..."
    buffer append ")"
    buffer toString
  }

  /**
    * TODO unit test
    *
    * @param f a function which transforms a T into an R
    * @tparam T the uderlying source type
    * @tparam R the underlying result type
    * @return a named function which takes a T and returns an R
    */
  def named[T, R](name: String, f: T => R) = new ((T) => R) {
    override def apply(v1: T): R = {
      println(s"applying $name to $v1"); f(v1)
    }

    override def toString = name
  }

  /**
    * Method to convert a b into an Option[X]
    * @param b a Boolean value
    * @param x an X value
    * @tparam X the uderlying type
    * @return if b is true then Some(x) else None
    */
  def toOption[X](b: Boolean, x: X) = if (b) Some(x) else None
}