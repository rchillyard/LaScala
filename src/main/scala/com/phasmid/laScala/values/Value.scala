package com.phasmid.laScala.values

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.fp.FP
import com.phasmid.laScala.values.Orderable.OrderableLocalDate

import scala.language.implicitConversions
import scala.util._

/**
  * Trait Value.
  *
  * The purpose of this trait is to be able to represent quantities -- things that have some ordered (and/or numerical)
  * value -- (or sequences of quantities) in a more meaningful way than simply using "Any".
  *
  * This trait defines seven methods: source, asBoolean, asValuable, asOrderable, asIncrementable, asFractional, asSequence.
  *
  * For values that you want to consider as numeric, then use asFractional. Valuable is very similar to Numeric/Fractional
  * but has additional methods, in particular fromString, and these methods typically result in a Try[X] as opposed to X.
  *
  * For more detail, see Scalar
  *
  * You can also represent sequences by Value (in particular, SequenceValue). Such values will yield Some(sequence)
  * when asSequence is invoked. Other types of Value will yield None in this situation.
  *
  * In the vast majority of cases, you can simply provide a String as the input to Value.apply.
  * This is the normal mechanism when you are reading values from HTML, JSON, Config, whatever.
  * Values that derive from String representations are normally StringValues
  * These typically result in an appropriate value when asValuable is invoked, that's to say --
  * not the String value but the parsed numeric (Valuable) value.
  * If you have a String that you want only to be considered a String, then use QuotedStringValue
  *
  * Created by scalaprof on 7/8/16.
  *
  */
sealed trait Value extends Scalar {

  /**
    * View this Value as a Sequence of Value objects.
    *
    * @return either Some(sequence) or None, as appropriate.
    */
  def asSequence: Option[Seq[Value]] = None
}

trait ValueMaker extends (Any => Try[Value]) {
  def apply(a: Any): Try[Value] = a match {
    case as: Seq[Any] => for (vs <- sequence(as)) yield SequenceValue(vs, as)
    case x => value(x)
  }

  def value(a: Any): Try[Value]

  def sequence(as: Seq[Any]): Try[Seq[Value]] = FP.sequence(as map apply)
}

/**
  * Value which is natively an Int. Such a value can be converted to Double by invoking asValuable[Double]
  *
  * @param x      the Int value
  * @param source the source (which could, conceivably, be a String)
  */
case class IntValue(x: Int, source: Any) extends BaseIntScalar(x, source) with Value

/**
  * Value which is natively a Boolean. Such a value can be converted to Int by invoking asValuable[Int] in which case
  * the result will be 1 for true and 0 for false.
  *
  * @param x      the Int value
  * @param source the source (which could, conceivably, be a String)
  */
case class BooleanValue(x: Boolean, source: Any) extends BaseBooleanScalar(x, source) with Value

/**
  * Value which is natively a Double. Such a value cannot be converted to Int by invoking asValuable[Int] because of
  * loss of precision.
  *
  * @param x      the Double value
  * @param source the source (which could, conceivably, be a String)
  */
case class DoubleValue(x: Double, source: Any) extends BaseDoubleScalar(x, source) with Value

/**
  * Value which is natively a Double. Such a value cannot be converted to Int by invoking asValuable[Int] because of
  * loss of precision.
  *
  * @param x      the Double value
  * @param source the source (which could, conceivably, be a String)
  */
case class RationalValue(x: LongRational, source: Any) extends BaseRationalScalar(x, source) with Value

/**
  * Value which is natively a String. Such a value, providing it is formatted appropriately, can be converted to Int or
  * Double by invoking asValuable[Int] or asValuable[Double], respectively.
  *
  * @param x      the String value
  * @param source the source (normally a String)
  */
case class StringValue(x: String, source: Any) extends BaseStringScalar(x, source) with Value

/**
  * Value which is natively a String. Such a value cannot be converted to Int or
  * Double by invoking asValuable.
  *
  * @param x      the String value
  * @param source the source (normally a String but might be enclosed in quotation marks)
  */
case class QuotedStringValue(x: String, source: Any) extends BaseQuotedStringScalar(x, source) with Value {
  // CONSIDER creating a sequence of Char?
  override def asSequence: Option[Seq[Value]] = None
}

/**
  * Value which is natively an LocalDate. Such a value, cannot be converted to Int or
  * Double by invoking asValuable. However, it can be converted to some other form of Date by invoking
  * asOrderable[X] where X is the other form--provided that there is an implicit conversion function in scope.
  *
  * @param x      the LocalDate value
  * @param source the source (normally a String)
  */
case class DateValue(x: LocalDate, source: Any) extends BaseDateScalar(x, source) with Value

/**
  * Value which is actually a Seq of Values. Such a value, cannot be converted to Int or
  * Double by invoking asValuable, nor by invoking asOrderable. However, when tested by invoking asSequence, such values
  * result in Some(sequence).
  *
  * @param xs     the Seq of Values
  * @param source the source (typically, a Seq of Any objects)
  */
case class SequenceValue(xs: Seq[Value], source: Any) extends BaseScalar(xs, source) with Value {
  override def asSequence: Option[Seq[Value]] = Some(xs)

  override def toString: String = xs.toString

  def defaultFormat: String = null
}

class ValueException(s: String, t: scala.Throwable = null) extends Exception(s, t)

// CONSIDER how do we re-use the code from Scalar in the following methods?
object BooleanValue {
  def apply(x: Boolean): BooleanValue = BooleanValue(x, x)
}

object IntValue {
  def apply(x: Int): IntValue = IntValue(x, x)
}

object DoubleValue {
  def apply(x: Double): DoubleValue = DoubleValue(x, x)
}

object RationalValue {
  def apply(x: LongRational): RationalValue = RationalValue(x, x)
}

object StringValue {
  def apply(x: String): StringValue = StringValue(x, x)
}

object QuotedStringValue {
  def apply(x: String): QuotedStringValue = QuotedStringValue(x, x)
}

object DateValue {
  def apply(x: LocalDate): DateValue = DateValue(x, x)

  def apply(x: String)(implicit pattern: String): DateValue = DateValue(LocalDate.parse(x, if (pattern.isEmpty) DateTimeFormatter.ISO_LOCAL_DATE else OrderableLocalDate.formatter(pattern)), x)

  def apply(y: Int, m: Int, d: Int): DateValue = apply(LocalDate.of(y, m, d))
}

object SequenceValue {
  def apply(xs: Seq[Any])(implicit conv: ValueMaker): SequenceValue = SequenceValue(Value.sequence(xs)(conv), xs)
}

object Value {

  implicit def apply(x: Boolean): Value = BooleanValue(x)

  implicit def apply(x: Int): Value = IntValue(x)

  implicit def apply(x: Double): Value = DoubleValue(x, x)

  implicit def apply(x: LongRational): Value = RationalValue(x, x)

  implicit def apply(x: String): Value = x match {
    case quoted(z) => QuotedStringValue(z, x)
    case _ => StringValue(x, x)
  }

  implicit def apply(x: LocalDate): Value = DateValue(x, x)

  implicit def apply(xs: Seq[Any]): Value = SequenceValue(xs)

  implicit val standardConverter: ValueMaker = new ValueMaker {
    def value(x: Any): Try[Value] = tryValue(x)
  }

  /**
    * Method to convert any of several types of object into a Value.
    * XXX Surely the implicits mechanism should be doing this for us!
    *
    * @param x an Any
    * @return a Try[Value] where the value is the result of applying one of the several apply methods in this Value object.
    */
  def tryValue(x: Any): Try[Value] = x match {
    case b: Boolean => Try(apply(b))
    case i: Int => Try(apply(i))
    case d: Double => Try(apply(d))
    case r: LongRational@unchecked => Try(apply(r))
    case w: String => Try(apply(w))
    case d: LocalDate => Try(apply(d))
    case xs: Seq[Any] => Try(apply(xs))
    // XXX shouldn't really need the following...
    case v: Value => Success(v)
    case _ => Failure(new ValueException(s"cannot form Value from type ${x.getClass}"))
  }

  /**
    * Transform a sequence of Anys into a sequence of corresponding Values
    *
    * @param ws a sequence of Anys
    * @return a sequence of Values
    */
  def sequence(ws: Seq[Any])(implicit conv: ValueMaker): Seq[Value] = {
    trySequence(ws)(conv) match {
      case Success(as) => as
      case Failure(x) => throw new ValueException(s"cannot form sequence of Values from given sequence $ws", x)
    }
  }

  /**
    * Transform a Map of Strings into a Map of corresponding Values.
    *
    * XXX note that there is no native Value which is a Map.
    *
    * @param kWm a map of Strings
    * @tparam K the key type
    * @return a map of Values
    */
  def sequence[K](kWm: Map[K, Any])(implicit conv: ValueMaker): Map[K, Value] = {
    // CONSIDER using mapValues
    val vtKs = (for ((k, v) <- kWm) yield (k, conv(v))).toSeq
    FP.sequence(for ((k, vt) <- vtKs) yield for (v <- vt) yield (k, v)) match {
      case Success(m) => m.toMap
      case Failure(x) => throw new ValueException(s"cannot form sequence of Values from given sequence", x)
    }
  }

  /**
    * Transform a sequence of Anys into a Sequence of corresponding Values
    *
    * @param ws a sequence of Anys
    * @return a sequence of Values
    */
  def trySequence(ws: Seq[Any])(implicit conv: ValueMaker): Try[Seq[Value]] = conv.sequence(ws)

  /**
    * Transform a Map of K,Any into a Map of corresponding K,Value
    *
    * @param kWm a map of K,Any
    * @tparam K the key type
    * @return a map of Values
    */
  def trySequence[K](kWm: Map[K, Any])(implicit conv: ValueMaker): Try[Map[K, Value]] = for (
    // CONSIDER using mapValues
    kVs <- FP.sequence((for ((k, v) <- kWm) yield for (z <- conv(v)) yield (k, z)).toSeq)
  ) yield kVs.toMap

  private val quoted = """"([^"]*)"""".r
}


