package com.phasmid.laScala.fp

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.util._
import scala.util.control.NonFatal

/**
  * This is a collection of general functional-programming-style methods.
  * We could probably get these from ScalaZ but I'd rather have no dependencies.
  *
  * TODO flesh out the unit tests (coverage is low).
  *
  * CONSIDER replace Seq with Iterator in signatures
  *
  * CONSIDER use higher-kinded monad types in order to implement methods for Try and Option at the same time
  *
  * @author scalaprof
  */
object FP {

  def flatten[X](xyf: Future[Try[X]])(implicit executor: ExecutionContext): Future[X] = for (xy <- xyf; x <- asFuture(xy)) yield x

  /**
    * this isn't a particularly useful method: basically, it just strips away the Try part, returning an un-fulfilled Future.
    *
    * @param xfy a Future[X] value wrapped in Try
    * @tparam X the underlying type
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
    * @param ec   the execution context
    * @tparam X the underlying type
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
    * @tparam X the underlying type
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
    * @tparam X the underlying type
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
    *
    * @param xe an Either[Throwable,X] object
    * @tparam X the underlying type
    * @return an X value wrapped inside Option
    */
  def sequence[X](xe: Either[Throwable, X]): Option[X] = xe.right.toOption

  /**
    * Perform recover on a sequence of Try objects.
    * @param xys the sequence of tries
    * @param t the Throwable to replace the nonfatal throwable already reported.
    * @tparam X the underlying type of the tries
    * @return xys essentially unchanged though with the side-effect of having written any non-fatal exceptions to the console and all non-fatal failures replaced by Failure(t)
    */
  def recoverWith[X](xys: Seq[Try[X]], t: Throwable = new java.util.NoSuchElementException): Seq[Try[X]] = xys map (xy => xy.recoverWith({ case NonFatal(x) => System.err.println(x.getLocalizedMessage); Failure(t) }))

  /**
    * TODO unit test
    * zip two options into an option of a tuple
    *
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
    *
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
    *
    * @param af an A wrapped inside Future
    * @param bf a B wrapped inside Future
    * @tparam A the underlying type of the first parameter
    * @tparam B the underlying type of the second parameter
    * @return a tuple containing an A and a B value, all wrapped inside Future
    */
  def zip[A, B](af: Future[A], bf: Future[B])(implicit ec: ExecutionContext): Future[(A, B)] = for (a <- af; b <- bf) yield (a, b)

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

  implicit val limit = 25

  /**
    * TODO unit test
    *
    * @param as    a sequence of As
    * @param limit the limit of how many you want to show
    * @tparam A the underlying type of the sequence
    * @return a String representing the first "limit" elements of as
    */
  def renderLimited[A](as: => Seq[A])(implicit limit: Int = as.length * 5): String = {
    val iterator = as.toStream.toIterator
    val buffer = new StringBuilder("(")
    while (iterator.hasNext && buffer.length < limit) {
      if (buffer.length > 1) buffer append ", "
      buffer append s"${iterator.next}"
    }
    if (iterator.hasNext) buffer append "..."
    buffer append ")"
    buffer toString
  }

  /**
    * TODO unit test
    *
    * @param f a function which transforms a T into an R
    * @tparam T the underlying source type
    * @tparam R the underlying result type
    * @return a named function which takes a T and returns an R
    */
  def named[T, R](name: String, f: T => R) = new ((T) => R) {
    def apply(v1: T): R = f(v1)

    override def toString = name
  }

  /**
    * Convert an Option[X] into a Try[X], given an explicit throwable for the None case
    *
    * @param xo an X value wrapped in Option
    * @param t  a throwable which, wrapped in Failure, will be returned if xo is None
    * @tparam X the underlying type of the input and the output
    * @return an X value wrapped as a Try
    */
  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = {
    val result = Try(xo.get)
    if (t != null) result.recoverWith { case e: java.util.NoSuchElementException => Failure(t) }
    else result
  }

  /**
    * Convert an Option[X] into a Try[X], using the default throwable for the None case
    *
    * @param xo an X value wrapped in Option
    * @tparam X the underlying type of the input and the output
    * @return an X value wrapped as a Try. If xo is None, then the result will be a NoSuchElementException wrapped in Failure
    */
  def optionToTry[X](xo: Option[X]): Try[X] = optionToTry(xo, null)

  /**
    * Alternative to the toOption method in Option.
    * This method will throw any fatal failures, log any non-fatal failures and then convert the Try to an Option
    * @param xy the Try object
    * @tparam X the underlying type of Try
    * @return an Option which is Some(x) for Success(x) and None for Failure(t) where t has been handled as a side-effect
    */
  def toOption[X](xy: Try[X], fLog: (Throwable) => Unit = { x => System.err.println(x.getLocalizedMessage)}): Option[X] = xy.recoverWith({
    case NonFatal(x) => fLog(x); Failure(new NoSuchElementException)
    case x @ _ => throw x
  }).toOption

  /**
    * Method to convert a b into an Option[X]
    *
    * @param b a Boolean value
    * @param x an X value
    * @tparam X the underlying type
    * @return if b is true then Some(x) else None
    */
  def toOption[X](b: Boolean, x: X): Option[X] = if (b) Some(x) else None

  /**
    * Method to yield an Option[X] value based on a X-predicate and an X value.
    * @param p the predicate
    * @param t the value to wrap in Some if the predicate is satisfied
    * @tparam X the underlying type of the value and the resulting Option
    * @return Option[X]
    */
  def toOption[X](p: X=>Boolean)(t: X): Option[X] = toOption(p(t),t)

  /**
    * TODO unit test
    *
    * method to map a pair of Option values (of same underlying type) into an Option value of another type (which could be the same of course)
    *
    * @param to1 a Option[T] value
    * @param to2 a Option[T] value
    * @param f   function which takes two T parameters and yields a U result
    * @tparam T the input type
    * @tparam U the result type
    * @return a Option[U]
    */
  def map2[T, U](to1: Option[T], to2: => Option[T])(f: (T, T) => U): Option[U] = for {t1 <- to1; t2 <- to2} yield f(t1, t2)

  /**
    * The map2 function. You already know this one!
    *
    * @param t1y parameter 1 wrapped in Try
    * @param t2y parameter 2 wrapped in Try
    * @param f   function that takes two parameters of types T1 and T2 and returns a value of R
    * @tparam T1 the type of parameter 1
    * @tparam T2 the type of parameter 2
    * @tparam R  the type of the result of function f
    * @return a value of R, wrapped in Try
    */
  def map2[T1, T2, R](t1y: Try[T1], t2y: Try[T2])(f: (T1, T2) => R): Try[R] =
  for {
    t1 <- t1y
    t2 <- t2y
  } yield f(t1, t2)

  /**
    * The map3 function. Much like map2
    *
    * @param t1y parameter 1 wrapped in Option
    * @param t2y parameter 2 wrapped in Option
    * @param t3y parameter 3 wrapped in Option
    * @param f   function that takes three parameters of types T1, T2 and T3 and returns a value of R
    * @tparam T1 the type of parameter 1
    * @tparam T2 the type of parameter 2
    * @tparam T3 the type of parameter 3
    * @tparam R  the type of the result of function f
    * @return a value of R, wrapped in Try
    */
  def map3[T1, T2, T3, R](t1y: Option[T1], t2y: Option[T2], t3y: Option[T3])(f: (T1, T2, T3) => R): Option[R] =
  for {t1 <- t1y
       t2 <- t2y
       t3 <- t3y
  } yield f(t1, t2, t3)

  /**
    * The map3 function. Much like map2
    *
    * @param t1y parameter 1 wrapped in Try
    * @param t2y parameter 2 wrapped in Try
    * @param t3y parameter 3 wrapped in Try
    * @param f   function that takes three parameters of types T1, T2 and T3 and returns a value of R
    * @tparam T1 the type of parameter 1
    * @tparam T2 the type of parameter 2
    * @tparam T3 the type of parameter 3
    * @tparam R  the type of the result of function f
    * @return a value of R, wrapped in Try
    */
  def map3[T1, T2, T3, R](t1y: Try[T1], t2y: Try[T2], t3y: Try[T3])(f: (T1, T2, T3) => R): Try[R] =
  for {t1 <- t1y
       t2 <- t2y
       t3 <- t3y
  } yield f(t1, t2, t3)

  /**
    * You get the idea...
    */
  def map7[T1, T2, T3, T4, T5, T6, T7, R](t1y: Try[T1], t2y: Try[T2], t3y: Try[T3], t4y: Try[T4], t5y: Try[T5], t6y: Try[T6], t7y: Try[T7])(f: (T1, T2, T3, T4, T5, T6, T7) => R): Try[R] =
  for {t1 <- t1y
       t2 <- t2y
       t3 <- t3y
       t4 <- t4y
       t5 <- t5y
       t6 <- t6y
       t7 <- t7y
  } yield f(t1, t2, t3, t4, t5, t6, t7)

  /**
    * TODO unit test
    *
    * method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
    *
    * @param ty1     a Try[T] value
    * @param ty2     a Try[T] value passed as call-by-name
    * @param f       function which takes two T parameters and yields a U result
    * @param g       (implicit) guard function which, given the first parameter's value, must be true for the second parameter (ty2) to be evaluated
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
    * method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
    *
    * @param ty1     a Try[T] value
    * @param ty2     a Try[T] value passed as call-by-name
    * @param ty3     a Try[T] value passed as call-by-name
    * @param f       function which takes two T parameters and yields a U result
    * @param g       (implicit) guard function which, given the first parameter's value, must be true for the second parameter (ty2) to be evaluated;
    *                and which, given the second parameter's value, must be true for the third parameter (ty3) to be evaluated
    * @param default (implicit) a default value
    * @tparam T the input type
    * @tparam U the result type
    * @return a Try[U]
    */
  def map3lazy[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U)(implicit g: T => Boolean = { x: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
  (for {t1 <- ty1; if g(t1); t2 <- ty2; if g(t2); t3 <- ty3} yield f(t1, t2, t3)) recoverWith { case z: java.util.NoSuchElementException => default }

  /**
    * Lift function to transform a function f of type T=>R into a function of type Try[T]=>Try[R]
    *
    * @param f the function we start with, of type T=>R
    * @tparam T the type of the parameter to f
    * @tparam R the type of the result of f
    * @return a function of type Try[T]=>Try[R]
    */
  def lift[T, R](f: T => R): Try[T] => Try[R] = _ map f

  /**
    * Lift function to transform a function f of type (T1,T2)=>R into a function of type (Try[T1],Try[T2])=>Try[R]
    *
    * @param f the function we start with, of type (T1,T2)=>R
    * @tparam T1 the type of the first parameter to f
    * @tparam T2 the type of the second parameter to f
    * @tparam R  the type of the result of f
    * @return a function of type (Try[T1],Try[T2])=>Try[R]
    */
  def lift2[T1, T2, R](f: (T1, T2) => R): (Try[T1], Try[T2]) => Try[R] = map2(_, _)(f)

  /**
    * Lift function to transform a function f of type (T1,T2,T3)=>R into a function of type (Try[T1],Try[T2],Try[T3])=>Try[R]
    *
    * @param f the function we start with, of type (T1,T2,T3)=>R
    * @tparam T1 the type of the first parameter to f
    * @tparam T2 the type of the second parameter to f
    * @tparam T3 the type of the third parameter to f
    * @tparam R  the type of the result of f
    * @return a function of type (Try[T1],Try[T2],Try[T3])=>Try[R]
    */
  def lift3[T1, T2, T3, R](f: (T1, T2, T3) => R): (Try[T1], Try[T2], Try[T3]) => Try[R] = map3(_, _, _)(f)

  /**
    * Lift function to transform a function f of type (T1,T2,T3,T4,T5,T6,T7)=>R into a function of type (Try[T1],Try[T2],Try[T3],Try[T4],Try[T5],Try[T6],Try[T7])=>Try[R]
    *
    * @param f the function we start with, of type (T1,T2,T3,T4,T5,T6,T7)=>R
    * @tparam T1 the type of the first parameter to f
    * @tparam T2 the type of the second parameter to f
    * @tparam T3 the type of the third parameter to f
    * @tparam T4 the type of the fourth parameter to f
    * @tparam T5 the type of the fifth parameter to f
    * @tparam T6 the type of the sixth parameter to f
    * @tparam T7 the type of the seventh parameter to f
    * @tparam R  the type of the result of f
    * @return a function of type (Try[T1],Try[T2],Try[T3],Try[T4],Try[T5],Try[T6],Try[T7])=>Try[R]
    */
  def lift7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R): (Try[T1], Try[T2], Try[T3], Try[T4], Try[T5], Try[T6], Try[T7]) => Try[R] = map7(_, _, _, _, _, _, _)(f)

  /**
    * This method inverts the order of the first two parameters of a two-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam R  the result type
    * @return a curried function which takes the second parameter first
    */
  def invert2[T1, T2, R](f: T1 => T2 => R): T2 => T1 => R = { t2 => { t1 => f(t1)(t2) } }

  /**
    * TODO unit test
    *
    * A true "lift" method which takes a function f (T=>U) and returns a Try[T]=>Try[T]
    *
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
    * @tparam T the underlying source type
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
    * @tparam T the underlying source type
    * @tparam U the underlying result type
    * @return the corresponding function which takes to Option[T] parameters and returns an Option[U]
    */
  def liftOption[T, U](f: T => U): Option[T] => Option[U] = _ map f

  /**
    * TODO unit test
    *
    * A pure lift method to lift a function into Future
    *
    * @param f        a function which takes two T parameters and returns a U
    * @param executor (implicit) execution context
    * @tparam T the underlying source type
    * @tparam U the underlying result type
    * @return the corresponding function which takes to Future[T] parameters and returns a Future[U]
    */
  def liftFuture[T, U](f: T => U)(implicit executor: ExecutionContext): Future[T] => Future[U] = _ map f

  /**
    * A true "lift" method which takes a function f (T=>U) and returns an Option[T]=>Try[T]
    * This is a bit of an odd-ball function, not frequently used.
    *
    * @param f a function which transforms an T into a U
    * @tparam T the T type
    * @tparam U the U type
    * @return the corresponding function which takes an Option[T] and returns a Try[U]
    */
  def liftOptionTry[T, U](f: T => U)(implicit ex: Throwable = null): Option[T] => Try[U] = {
    xo: Option[T] =>
      val result = for (x <- xo) yield f(x)
      if (ex != null) optionToTry(result, ex)
      else optionToTry(result)
  }


  /**
    * This method inverts the order of the first three parameters of a three-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam R  the result type
    * @return a curried function which takes the third parameter first, then the second, etc.
    */
  def invert3[T1, T2, T3, R](f: T1 => T2 => T3 => R): T3 => T2 => T1 => R = { t3 => { t2 => { t1 => f(t1)(t2)(t3) } } }

  /**
    * This method inverts the order of the first four parameters of a four-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam R  the result type
    * @return a curried function which takes the fourth parameter first, then the third, etc.
    */
  def invert4[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): T4 => T3 => T2 => T1 => R = { t4 => { t3 => { t2 => { t1 => f(t1)(t2)(t3)(t4) } } } }

  /**
    * This method uncurries the first two parameters of a three- (or more-)
    * parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first two parameters of f;
    * whose second parameter is the third parameter, etc.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2)=>T4=>R
    */
  def uncurried2[T1, T2, T3, R](f: T1 => T2 => T3 => R): (T1, T2) => T3 => R = { (t1, t2) => { t3 => f(t1)(t2)(t3) } }

  /**
    * This method uncurries the first three parameters of a four- (or more-)
    * parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first three parameters of f;
    * whose second parameter is the third parameter, etc.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3)=>T4=>R
    */
  def uncurried3[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): (T1, T2, T3) => T4 => R = { (t1, t2, t3) => { t4 => f(t1)(t2)(t3)(t4) } }

  /**
    * This method uncurries the first three parameters of a four- (or more-)
    * parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first seven parameters of f;
    * whose second parameter is the third parameter, etc.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3)=>T4=>R
    */
  def uncurried7[T1, T2, T3, T4, T5, T6, T7, T8, R](f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => R): (T1, T2, T3, T4, T5, T6, T7) => T8 => R =
  { (t1, t2, t3, t4, t5, t6, t7) => { t8 => f(t1)(t2)(t3)(t4)(t5)(t6)(t7)(t8) } }

}