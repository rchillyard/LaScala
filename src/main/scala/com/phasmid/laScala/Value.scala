package com.phasmid.laScala

import com.phasmid.laScala.parser.Valuable

import scala.util.Try

/**
  * Type class Versatile.
  *
  * Created by scalaprof on 7/8/16.
  */
trait Value[V] {

  def asValuable[X: Valuable]: Option[X]

  def source: V
}

case class IntValue(x: Int) extends Value[Int] {
  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromInt(x).toOption

  def source: Int = x
}

case class DoubleValue(x: Double) extends Value[Double] {
  // XXX this gives us the effect we want -- but is it right?
  def asValuable[X: Valuable]: Option[X] = Try(implicitly[Valuable[X]].unit(x.asInstanceOf[X])).toOption

  def source: Double = x
}

case class StringValue(x: String) extends Value[String] {
  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromString(x).toOption

  def source: String = x
}


object Value {

  def apply(x: Int) = IntValue(x)

  def apply(x: Double) = DoubleValue(x)

  def apply(x: String) = StringValue(x)
}


