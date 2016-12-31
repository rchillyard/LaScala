package com.phasmid.laScala.fp

import scala.language.{higherKinds, postfixOps}
import scala.util._

/**
  * This is an 2.10-specific appendix to FP
  *
  * @author scalaprof
  */
object FP_Cross {

  /**
    * This method is only required when compiling with Scala 2.10.
    * @param ox the given Option
    * @param x the value we want to compare with ox
    * @tparam X the underlying type
    * @return true if ox is Some(x)
    */
  def contains[X](ox: Option[X], x: X): Boolean = !ox.isEmpty && ox.get == x

    /**
      * Method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
      *
      * NOTE: this implementation (for 2.10) simply invokes the non-optimized version of map2
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
    def map2lazy[T, U](ty1: Try[T], ty2: => Try[T])(f: (T, T) => U)(implicit g: T => Boolean = { _: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
      FP.map2(ty1,ty2)(f)

  /**
    * Method to map a pair of Try values (of same underlying type) into a Try value of another type (which could be the same of course)
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
  def map3lazy[T, U](ty1: Try[T], ty2: => Try[T], ty3: => Try[T])(f: (T, T, T) => U)(implicit g: T => Boolean = { _: T => true }, default: Try[U] = Failure[U](new Exception("no default result specified"))): Try[U] =
    FP.map3(ty1,ty2,ty3)(f)
}