package com.phasmid.laScala

import java.time.LocalDate

import com.phasmid.laScala.parser.Valuable

import scala.util.{Failure, Try}

/**
  * Trait Value.
  *
  * This trait defines two methods: source and asValuable.
  *
  * Values that derive from String representations are normally StringValues
  * These typically result in an appropriate value when asValuable is invoked, that's to say --
  * not the String value but the parsed numeric (Valuable) value.
  * If you have a String that you want only to be considered a String, then use QuotedStringValue
  *
  * Created by scalaprof on 7/8/16.
  */
sealed trait Value {

  /**
    * Transform this Value into an (optional) X value which is Orderable
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
  def asOrderable[X: Orderable](implicit pattern: String): Option[X]

  /**
    * Transform this Value into an (optional) X value which is Valuable
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
  def asValuable[X: Valuable]: Option[X]

  def source: Any
}

case class IntValue(x: Int, source: Any) extends Value {
  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromInt(x).toOption

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  override def toString = x.toString
}

case class DoubleValue(x: Double, source: Any) extends Value {
  // XXX this gives us the effect we want -- but is it right?
  def asValuable[X: Valuable]: Option[X] = Try(implicitly[Valuable[X]].unit(x.asInstanceOf[X])).toOption

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  override def toString = x.toString
}

case class StringValue(x: String, source: Any) extends Value {
  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromString(x)("").toOption

  def asOrderable[X: Orderable](implicit pattern: String): Option[X] = implicitly[Orderable[X]].fromString(x)(pattern).toOption

  override def toString = x.toString
}

case class QuotedStringValue(x: String, source: Any) extends Value {
  def asValuable[X: Valuable]: Option[X] = None

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  override def toString = x.toString
}

case class DateValue(x: LocalDate, source: Any) extends Value {
  def asValuable[X: Valuable]: Option[X] = None

  def asOrderable[X: Orderable](implicit pattern: String): Option[X] = Try(implicitly[Orderable[X]].unit(x.asInstanceOf[X])).toOption

  override def toString = source.toString
}

class ValueException(s: String) extends Exception(s)

object IntValue {
  def apply(x: Int): IntValue = IntValue(x, x)
}

object DoubleValue {
  def apply(x: Double): DoubleValue = DoubleValue(x, x)
}

object StringValue {
  def apply(x: String): StringValue = StringValue(x, x)
}

object QuotedStringValue {
  def apply(x: String): QuotedStringValue = QuotedStringValue(x, x)
}

object DateValue {
  def apply(x: LocalDate): DateValue = DateValue(x, x)

  def apply(y: Int, m: Int, d: Int): DateValue = apply(LocalDate.of(y, m, d))
}

object Value {

  def apply(x: Int) = IntValue(x)

  def apply(x: Double) = DoubleValue(x, x)

  def apply(x: String) = x match {
    case quoted(z) => QuotedStringValue(z, x)
    case _ => StringValue(x, x)
  }

  def tryValue(x: Any): Try[Value] = x match {
    case i: Int => Try(apply(i))
    case d: Double => Try(apply(d))
    case w: String => Try(apply(w))
    case _ => Failure(new ValueException(s"cannot form Value from type ${x.getClass}"))
  }

  /**
    * Transform a sequence of Strings into a Sequence of corresponding Values
    *
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
  def sequence[K](kWm: Map[K, String]): Map[K, Value] = kWm mapValues {
    Value.apply(_)}

  /**
    * Transform a sequence of Strings into a Sequence of corresponding Values
    *
    * @param ws a sequence of Strings
    * @return a sequence of Values
    */
  def trySequence(ws: Seq[Any]): Try[Seq[Value]] = FP.sequence(ws map {
    Value.tryValue(_)})

  /**
    * Transform a Map of Strings into a Map of corresponding Values
    *
    * @param kWm a map of Strings
    * @tparam K the key type
    * @return a map of Values
    */
  def trySequence[K](kWm: Map[K, Any]): Try[Map[K, Value]] = for (
    kVs <- FP.sequence((for ((k,v) <- kWm) yield for (z <- Value.tryValue(v)) yield (k, z)).toSeq)
  ) yield kVs.toMap

  val quoted = """"([^"]*)"""".r
}


