package com.phasmid.laScala

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.util._

/**
  * @author scalaprof
  */
object FP {

  // TODO implement. 6 points. Hint: write as a for-comprehension, using the method asFuture (below).
  def flatten[X](xyf: Future[Try[X]])(implicit executor: ExecutionContext): Future[X] = for (xy <- xyf; x <- asFuture(xy)) yield x

  /**
    * this isn't a particularly useful method: basically, it just strips away the Try part, returning an un-fulfilled Future.
    * @param xfy
    * @tparam X
    * @return
    */
  def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

  // TODO implement. 6 points. Hint: write as a for-comprehension, using the method Future.sequence
  def flatten[X](xsfs: Seq[Future[Seq[X]]])(implicit ec: ExecutionContext): Future[Seq[X]] = Future.sequence(xsfs) map {
    _ flatten
  }

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

//  def flatten[X](xfy: Try[Future[X]]): Future[X] =
//    xfy match {
//      case Success(xf) => xf
//      case Failure(e) => (Promise[X] complete (throw e)).future
//    }

  def flatten[K, V](voKm: Map[K, Option[V]]): Map[K, V] = for ((k, vo) <- voKm; v <- vo) yield k -> v


  def asFuture[X](xy: Try[X]): Future[X] = xy match {
    case Success(s) => Future.successful(s)
    case Failure(e) => Future.failed(e)
  }

//  def sequence[M[_], N[_], T](in: M[N[T]]): N[M[T]] = ???

  // TODO implement. 4 points. 
  def sequence[X](xy: Try[X]): Either[Throwable, X] =
    xy match {
      case Success(s) => Right(s)
      case Failure(e) => Left(e)
    }

  def sequence[X](xf: Future[X])(implicit executor: ExecutionContext): Future[Either[Throwable, X]] =
    xf transform( { s => Right(s) }, { f => f }) recoverWith[Either[Throwable, X]] { case f => Future(Left(f)) }

  // TODO implement. 6 points. Hint: write as a for-comprehension, using the method sequence (above). 
  def sequence[X](xfs: Seq[Future[X]])(implicit executor: ExecutionContext): Seq[Future[Either[Throwable, X]]] = for (xf <- xfs) yield sequence(xf)

  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = (Try(Seq[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  def sequence[X](xys: Stream[Try[X]]): Try[Stream[X]] = (Try(Stream[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = (Option(Seq[X]()) /: xos) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  // TODO implement. 7 points. This one is a little more tricky. Remember what I mentioned about Either not being a pure monad -- it needs projecting
  def sequence[X](xe: Either[Throwable, X]): Option[X] = xe.right.toOption

  def zip[A, B](ao: Option[A], bo: Option[B]): Option[(A, B)] = for (a <- ao; b <- bo) yield (a, b)

  def optionToTry[X](xo: Option[X], t: => Throwable): Try[X] = Try(xo.get).recoverWith{case z: java.util.NoSuchElementException => Failure[X](t)}

  def optionToTry[X](xo: Option[X]): Try[X] = Try(xo.get)

  def map2[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2} yield f(t1, t2)

  def map2lazy[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U)(implicit g: T=>Boolean = {x: T =>true}, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    (for {t1 <- ty1; if g(t1); t2 <- ty2} yield f(t1, t2)) recoverWith{case z: java.util.NoSuchElementException => default}

  // Alternative version of map2lazy
//  def map2lazy[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U)(implicit g: T=>Boolean = {x: T => true}, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
//    {
//      ty1 match {
//        case Success(t1) => if (g(t1)) {
//          ty2 match {
//            case Success(t2) => Try(f(t1,t2))
//            case Failure(x2) => Failure(x2)
//          }
//        }
//          else default
//        case Failure(x1) => Failure(x1)
//      }
//    }

  def map3[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U): Try[U] = for {t1 <- ty1; t2 <- ty2; t3 <- ty3} yield f(t1, t2, t3)

  def map3lazy[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U)(implicit g: T=>Boolean = {x: T =>true}, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    (for {t1 <- ty1; if g(t1); t2 <- ty2; if g(t2); t3 <- ty3} yield f(t1, t2, t3)) recoverWith{case z: java.util.NoSuchElementException => default}

  def lift[X, Y](f: X => Y)(xt: Try[X]): Try[Y] = xt map f

  def trial[X,Y](f: X => Y)(x: => X): Try[Y] = lift(f)(Try(x))

  def liftTry[T, U](f: T => U): Try[T]=>Try[U] = _ map f

  def lift2Try[T, U](f: (T,T) => U): (Try[T],Try[T])=>Try[U] = map2 (_,_) (f)

  def liftOption[T, U](f: T => U): Option[T]=>Option[U] = _ map f

  def liftFuture[T, U](f: T => U)(implicit executor: ExecutionContext): Future[T]=>Future[U] = _ map f

  implicit val limit = 25
  def renderLimited[A](as: => Seq[A])(implicit limit: Int): String = {
    val iter = as.toStream.toIterator
    val buffer = new StringBuilder("(")
    while (iter.hasNext && buffer.length<limit) {
      if (buffer.length > 1) buffer append ", "
      buffer append s"${iter.next}"
    }
    if (iter.hasNext) buffer append "..."
    buffer append ")"
    buffer toString
  }

  def named[T,R](name: String, f: T=>R) = new Function1[T,R] {
    override def apply(v1: T): R = {println(s"applying $name to $v1"); f(v1)}
    override def toString = name
  }
}