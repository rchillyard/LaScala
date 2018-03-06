package com.phasmid.laScala.fuzzy

import scala.reflect.ClassTag

trait Fuzziness[T, X] extends Probability[T, X] {
  /**
    * If T is a continuous variable, tfo will provide Some appropriate Fractional,
    * otherwise, it will be None.
    * @return
    */
  def tfo: Option[Fractional[T]]

}

case class ContinuousFuzziness[T: Fractional, X: Fractional](f: T=>X) extends Fuzziness[T, X] {
  /**
  * If T is a continuous variable, tfo will provide Some appropriate Fractional,
  * otherwise, it will be None.
  *
  * @return
  */
  def tfo: Option[Fractional[T]] = Some(implicitly[Fractional[T]])

  def isPdf: Boolean = true

  def apply(t: T): X = f(t)
}

object Fuzziness {
  def apply[T: Fractional, X: Fractional](f: T=>X): Fuzziness[T, X] = ContinuousFuzziness(f)
}

