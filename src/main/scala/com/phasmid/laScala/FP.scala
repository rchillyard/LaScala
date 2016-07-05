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
    * @param xfy
    * //    * @tparam X
    * @return
    */
  def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

  def flatten[X](xsfs: Seq[Future[Seq[X]]])(implicit ec: ExecutionContext): Future[Seq[X]] = Future.sequence(xsfs) map {
    _ flatten
  }

  /**
    * TODO unit test
    *
    * @param esf      the collection to process
    * @param f        the function to process left-hand-sides
    * @param executor the executor to use
    *                 //    * @tparam X
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

  def asFuture[X](xy: Try[X]): Future[X] = xy match {
    case Success(s) => Future.successful(s)
    case Failure(e) => Future.failed(e)
  }

  /**
    * TODO unit test
    *
    * @param xy
    * //    * @tparam X
    * @return
    */
  def sequence[X](xy: Try[X]): Either[Throwable, X] =
    xy match {
      case Success(s) => Right(s)
      case Failure(e) => Left(e)
    }

  def sequence[X](xf: Future[X])(implicit executor: ExecutionContext): Future[Either[Throwable, X]] =
    xf transform( { s => Right(s) }, { f => f }) recoverWith[Either[Throwable, X]] { case f => Future(Left(f)) }

  def sequence[X](xfs: Seq[Future[X]])(implicit executor: ExecutionContext): Seq[Future[Either[Throwable, X]]] = for (xf <- xfs) yield sequence(xf)

  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = (Try(Seq[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
    * TODO unit test
    *
    * @param xys
    * //    * @tparam X
    * @return
    */
  def sequence[X](xys: Stream[Try[X]]): Try[Stream[X]] = (Try(Stream[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
    * TODO unit test
    *
    * @param xos
    * //    * @tparam X
    * @return
    */
  def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = (Option(Seq[X]()) /: xos) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  def sequence[X](xe: Either[Throwable, X]): Option[X] = xe.right.toOption

  def zip[A, B](ao: Option[A], bo: Option[B]): Option[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  def optionToTry[T](xo: Option[T], x: => Throwable): Try[T] = Try(xo.get).recoverWith { case e: java.util.NoSuchElementException => Failure[T](x) }

  def optionToTry[X](xo: Option[X]): Try[X] = Try(xo.get)

  def map2[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2} yield f(t1, t2)

  def map2lazy[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U)(implicit g: T => Boolean = { x: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    (for {t1 <- ty1; if g(t1); t2 <- ty2} yield f(t1, t2)) recoverWith { case z: java.util.NoSuchElementException => default }

  def map3[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2; t3 <- ty3} yield f(t1, t2, t3)

  def map3lazy[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U)(implicit g: T => Boolean = { x: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    (for {t1 <- ty1; if g(t1); t2 <- ty2; if g(t2); t3 <- ty3} yield f(t1, t2, t3)) recoverWith { case z: java.util.NoSuchElementException => default }

  /**
    * TODO unit test
    *
    * @param f
    * //    * @tparam X
    * @return
    */
  def lift[X, Y](f: X => Y)(xt: Try[X]): Try[Y] = xt map f

  /**
    * TODO unit test
    *
    * @param f
    * //    * @tparam X
    * @return
    */
  def trial[X, Y](f: X => Y)(x: => X): Try[Y] = lift(f)(Try(x))

  def liftTry[T, U](f: T => U): Try[T] => Try[U] = _ map f

  /**
    * TODO unit test
    *
    * @param f
    * //    * @tparam X
    * @return
    */
  def lift2Try[T, U](f: (T, T) => U): (Try[T], Try[T]) => Try[U] = map2(_, _)(f)

  /**
    * TODO unit test
    *
    * @param f
    * //    * @tparam X
    * @return
    */
  def liftOption[T, U](f: T => U): Option[T] => Option[U] = _ map f

  /**
    * TODO unit test
    *
    * @param f
    * //    * @tparam X
    * @return
    */
  def liftFuture[T, U](f: T => U)(implicit executor: ExecutionContext): Future[T] => Future[U] = _ map f

  implicit val limit = 25

  def renderLimited[A](as: => Seq[A])(implicit limit: Int): String = {
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
    * @param f
    * //    * @tparam T
    * @return
    */
  def named[T, R](name: String, f: T => R) = new ((T) => R) {
    override def apply(v1: T): R = {
      println(s"applying $name to $v1"); f(v1)
    }

    override def toString = name
  }

  def toOption[X](b: Boolean, x: X) = if (b) Some(x) else None
}