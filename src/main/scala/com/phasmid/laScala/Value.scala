package com.phasmid.laScala

import com.phasmid.laScala.parser.Valuable

import scala.util.{Failure, Try}

/**
  * Trait Value.
  *
  * This trait defines two methods: source and asValuable.
  *
  * Values that derive from String representations are normally StringValues
  * These typically result in an appropriate value when asValuable is invoked.
  * If you have a String that you do not want only to be considered a String, then use QuotedStringValue
  *
  * Created by scalaprof on 7/8/16.
  */
sealed trait Value[V] {

  /**
    * Transform this Value into an (optional) X value
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
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

case class QuotedStringValue(x: String) extends Value[String] {
  def asValuable[X: Valuable]: Option[X] = None

  def source: String = x
}

class ValueException(s: String) extends Exception(s)

object Value {

  def apply(x: Int) = IntValue(x)

  def apply(x: Double) = DoubleValue(x)

  def apply(x: String) = x match {
    case quoted(z) => QuotedStringValue(z)
    case _ => StringValue(x)
  }

  def tryValue(x: Any): Try[Value[_]] = x match {
    case i: Int => Try(apply(i))
    case d: Double => Try(apply(d))
    case w: String => Try(apply(w))
    case _ => Failure(new ValueException(s"cannot form Value from type ${x.getClass}"))
  }

  /**
    * Transform a sequence of Strings into a Sequence of corresponding Values
    * @param ws a sequence of Strings
    * @return a sequence of Values
    */
  def sequence(ws: Seq[String]) = ws map {Value.apply(_)}

  /**
    * Transform a Map of Strings into a Map of corresponding Values
    *
    * @param kWm a map of Strings
    * @tparam K the key type
    * @return a map of Values
    */
  def sequence[K](kWm: Map[K,String]): Map[K,Value[_]] = kWm mapValues {Value.apply(_)}

  /**
    * Transform a sequence of Strings into a Sequence of corresponding Values
    * @param ws a sequence of Strings
    * @return a sequence of Values
    */
  def trySequence(ws: Seq[Any]): Try[Seq[Value[_]]] = FP.sequence(ws map {Value.tryValue(_)})

  /**
    * Transform a Map of Strings into a Map of corresponding Values
    *
    * @param kWm a map of Strings
    * @tparam K the key type
    * @return a map of Values
    */
  def trySequence[K](kWm: Map[K,Any]): Try[Map[K,Value[_]]] = for (
    kVs <- FP.sequence((for ((k,v) <- kWm) yield for (z <- Value.tryValue(v)) yield (k, z)).toSeq)
  ) yield kVs.toMap

  val quoted = """"([^"]*)"""".r
}


