package com.phasmid.laScala.values

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.phasmid.laScala.Orderable.OrderableLocalDate
import com.phasmid.laScala.parser.Valuable
import com.phasmid.laScala.{FP, Incrementable, Orderable}

import scala.language.implicitConversions
import scala.util._

/**
  * Trait Scalar.
  *
  * The purpose of this trait is to be able to represent quantities -- things that have some ordered (and/or numerical)
  * value -- in a more meaningful way than simply using "Any".
  *
  * This trait defines five methods: source, asBoolean, asValuable, asOrderable, asIncrementable.
  *
  * For values that you want to consider as numeric, then use asValuable. Valuable is very similar to Numeric
  * but has additional methods, in particular fromString, and these methods typically result in a Try[X] as opposed to X.
  *
  * For values that you want to consider as orderable, then use asOrderable. Orderable extends Ordering.
  * It is used for the type of quantities that do not support arithmetic operations, but do support ordering.
  * A prime example is Date, Datetime, etc.
  *
  * In the vast majority of cases, you can simply provide a String as the input to Scalar.apply.
  * This is the normal mechanism when you are reading values from HTML, JSON, Config, whatever.
  * Scalars that derive from String representations are normally StringScalars
  * These typically result in an appropriate value when asValuable is invoked, that's to say --
  * not the String value but the parsed numeric (Valuable) value.
  * If you have a String that you want only to be considered a String, then use QuotedStringScalar
  *
  * Created by scalaprof on 8/4/16.
  *
  */
sealed trait Scalar {

  /**
    * Method to get the wrapped value as an Any
    *
    * @return the value as an Any
    */
  def get: Any

  /**
    * If this Scalar is a Boolean, then return Some(true) or Some(false) as appropriate.
    * Otherwise, return None.
    *
    * @return Some(true), Some(false) or None
    */
  def asBoolean: Option[Boolean]

  /**
    * Transform this Scalar into an (optional) X value which is Orderable
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
  def asOrderable[X: Orderable](implicit pattern: String): Option[X]

  /**
    * Transform this Scalar into an (optional) X value which is Incrementable
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
  def asIncrementable[X: Incrementable](implicit pattern: String): Option[X]

  /**
    * Transform this Scalar into an (optional) X value which is Valuable
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
  def asValuable[X: Valuable]: Option[X]

  /**
    * Transform this Scalar into an (optional) X value which is Fractional
    *
    * @tparam X the type of the result we desire
    * @return either Some(x) if this value can be represented so, without information loss;
    *         or None otherwise.
    */
  def asFractional[X: Fractional]: Option[X]

  /**
    * Method to return the source of this Scalar, which may or may not be different from the actual value.
    * In some situations, this may be helpful.
    *
    * @return the origin of this Scalar
    */
  def source: Any

  /**
    * Method to show this Scalar simply, obviously and elegantly.
    * If you're interested in the Scalar wrapper type, use toString.
    *
    * @return a String representing the value which, typically, does not include the wrapper type.
    */
  def render: String
}

trait ScalarMaker extends (Any => Try[Scalar]) {
  def apply(a: Any) = a match {
    case x => value(x)
  }

  def value(a: Any): Try[Scalar]
}

/**
  * Scalar which is natively an Int. Such a value can be converted to Double by invoking asValuable[Double]
  *
  * @param x      the Int value
  * @param source the source (which could, conceivably, be a String)
  */
case class IntScalar(x: Int, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = None

  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromInt(x).toOption

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = None

  def asFractional[X: Fractional]: Option[X] = Some(implicitly[Fractional[X]].fromInt(x))

  def get: Any = x

  override def toString = s"IntScalar: $render"

  def render = x.toString
}

/**
  * Scalar which is natively a Boolean. Such a value can be converted to Int by invoking asValuable[Int] in which case
  * the result will be 1 for true and 0 for false.
  *
  * @param x      the Int value
  * @param source the source (which could, conceivably, be a String)
  */
case class BooleanScalar(x: Boolean, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = Some(x)

  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromInt(if (x) 1 else 0).toOption

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = None

  def asFractional[X: Fractional]: Option[X] = None

  def get: Any = x

  override def toString = s"BooleanScalar: $render"

  def render = x.toString
}

/**
  * Scalar which is natively a Double. Such a value cannot be converted to Int by invoking asValuable[Int] because of
  * loss of precision.
  *
  * @param x      the Double value
  * @param source the source (which could, conceivably, be a String)
  */
case class DoubleScalar(x: Double, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = None

  // XXX this gives us the effect we want -- conversion to Double but not, e.g. Int.
  // However, it is not elegant.
  // We really should try to convert a Double to an Int, for example, and test there is no information loss.
  def asValuable[X: Valuable]: Option[X] = Try(implicitly[Valuable[X]].unit(x.asInstanceOf[X])).toOption

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = None

  def asFractional[X: Fractional]: Option[X] = {
    //    val fractional = implicitly[Fractional[X]]
    //    Some(fractional.times(x.asInstanceOf[X], fractional.one))
    Some(x.asInstanceOf[X])
  }

  def get: Any = x

  override def toString = s"DoubleScalar: $render"

  def render = x.toString
}

/**
  * Scalar which is natively a Double. Such a value cannot be converted to Int by invoking asValuable[Int] because of
  * loss of precision.
  *
  * @param x      the Double value
  * @param source the source (which could, conceivably, be a String)
  */
case class RationalScalar(x: Rational, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = None

  def asValuable[X: Valuable]: Option[X] = for (x1 <- DoubleScalar(x.n).asValuable; x2 <- DoubleScalar(x.d).asValuable; y <- implicitly[Valuable[X]].div(x1, x2).toOption) yield y

  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = None

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = None

  def asFractional[X: Fractional]: Option[X] = Some(x.asInstanceOf[X])

  def get: Any = x

  override def toString = s"RationalScalar: $render"

  def render = x.toString
}

/**
  * Scalar which is natively a String. Such a value, providing it is formatted appropriately, can be converted to Int or
  * Double by invoking asValuable[Int] or asValuable[Double], respectively.
  *
  * @param x      the String value
  * @param source the source (normally a String)
  */
case class StringScalar(x: String, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = None

  def asValuable[X: Valuable]: Option[X] = implicitly[Valuable[X]].fromString(x)("").toOption

  def asOrderable[X: Orderable](implicit pattern: String): Option[X] = implicitly[Orderable[X]].fromString(x)(pattern).toOption

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = implicitly[Incrementable[X]].fromString(x)(pattern).toOption

  def asFractional[X: Fractional]: Option[X] = Try(x.toDouble.asInstanceOf[X]).toOption

  def get: Any = x

  override def toString = s"StringScalar: $render"

  def render = x.toString
}

/**
  * Scalar which is natively a String. Such a value cannot be converted to Int or
  * Double by invoking asValuable.
  *
  * @param x      the String value
  * @param source the source (normally a String but might be enclosed in quotation marks)
  */
case class QuotedStringScalar(x: String, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = None

  def asValuable[X: Valuable]: Option[X] = None

  // TODO create a concrete implicit object for OrderableString
  def asOrderable[X: Orderable](implicit pattern: String = ""): Option[X] = Try(implicitly[Orderable[X]].unit(x.asInstanceOf[X])).toOption

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = None

  def asFractional[X: Fractional]: Option[X] = None

  def get: Any = x

  override def toString = s"QuoteStringScalar: $render"

  def render = x.toString
}

/**
  * Scalar which is natively an LocalDate. Such a value, cannot be converted to Int or
  * Double by invoking asValuable. However, it can be converted to some other form of Date by invoking
  * asOrderable[X] where X is the other form--provided that there is an implict conversion function in scope.
  *
  * @param x      the LocalDate value
  * @param source the source (normally a String)
  */
case class DateScalar(x: LocalDate, source: Any) extends BaseScalar(x, source) {
  // CONSIDER returning None here, because a Date value is inherently Incrementable
  override def asOrderable[X: Orderable](implicit pattern: String): Option[X] = Try(implicitly[Orderable[X]].unit(x.asInstanceOf[X])).toOption

  override def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = Try(implicitly[Incrementable[X]].unit(x.asInstanceOf[X])).toOption

  override def toString = source.toString

  override def equals(obj: scala.Any): Boolean = obj match {
    case o: DateScalar => x.equals(o.x)
    case _ => super.equals(obj)
  }

  override def hashCode(): Int = x.hashCode
}

abstract class BaseScalar(value: Any, source: Any) extends Scalar {
  def asBoolean: Option[Boolean] = None

  def asOrderable[X: Orderable](implicit pattern: String): Option[X] = None

  def asIncrementable[X: Incrementable](implicit pattern: String = ""): Option[X] = None

  def asValuable[X: Valuable]: Option[X] = None

  def asFractional[X: Fractional]: Option[X] = None

  def get: Any = value

  override def toString = source.toString

  def render = getClass.getSimpleName + get

  override def equals(obj: scala.Any): Boolean = obj match {
    case o: BaseScalar => get.equals(o.get)
    case _ => super.equals(obj)
  }

  override def hashCode(): Int = value.hashCode
}

class ScalarException(s: String, t: scala.Throwable = null) extends Exception(s, t)

object BooleanScalar {
  def apply(x: Boolean): BooleanScalar = BooleanScalar(x, x)
}

object IntScalar {
  def apply(x: Int): IntScalar = IntScalar(x, x)
}

object DoubleScalar {
  def apply(x: Double): DoubleScalar = DoubleScalar(x, x)
}

object RationalScalar {
  def apply(x: Rational): RationalScalar = RationalScalar(x, x)
}

object StringScalar {
  def apply(x: String): StringScalar = StringScalar(x, x)
}

object QuotedStringScalar {
  def apply(x: String): QuotedStringScalar = QuotedStringScalar(x, x)
}

object DateScalar {
  def apply(x: LocalDate): DateScalar = DateScalar(x, x)

  def apply(x: String)(implicit pattern: String): DateScalar = DateScalar(LocalDate.parse(x, if (pattern.isEmpty) DateTimeFormatter.ISO_LOCAL_DATE else OrderableLocalDate.formatter(pattern)), x)

  def apply(y: Int, m: Int, d: Int): DateScalar = apply(LocalDate.of(y, m, d))
}

object Scalar {

  implicit def apply(x: Boolean): Scalar = BooleanScalar(x)

  implicit def apply(x: Int): Scalar = IntScalar(x)

  implicit def apply(x: Double): Scalar = DoubleScalar(x, x)

  implicit def apply(x: Rational): Scalar = RationalScalar(x, x)

  implicit def apply(x: String): Scalar = x match {
    case quoted(z) => QuotedStringScalar(z, x)
    case _ => StringScalar(x, x)
  }

  implicit def apply(x: LocalDate): Scalar = DateScalar(x, x)

  implicit val standardConverter = new ScalarMaker {
    def value(x: Any): Try[Scalar] = tryScalar(x)
  }

  /**
    * Method to convert any of several types of object into a Scalar
    *
    * @param x an Any
    * @return a Try[Scalar] where the value is the result of applying one of the several apply methods in this Scalar object.
    */
  def tryScalar(x: Any): Try[Scalar] = x match {
    case b: Boolean => Try(apply(b))
    case i: Int => Try(apply(i))
    case d: Double => Try(apply(d))
    case r: Rational => Try(apply(r))
    case w: String => Try(apply(w))
    case d: LocalDate => Try(apply(d))
    // XXX shouldn't really need the following...
    case v: Scalar => Success(v)
    case _ => Failure(new ScalarException(s"cannot form Scalar from type ${x.getClass}"))
  }

  def tryScalarTuple(k: String, x: Any): Try[(String, Scalar)] = tryScalar(x) match {
    case Success(s) => Success(k, s)
    case Failure(t) => Failure(t)
  }

  /**
    * Transform a Map of Strings into a Map of corresponding Scalars.
    *
    * XXX note that there is no native Scalar which is a Map.
    *
    * @param kWm a map of Strings
    * @tparam K the key type
    * @return a map of Scalars
    */
  def sequence[K](kWm: Map[K, Any])(implicit conv: ScalarMaker): Map[K, Scalar] = {
    // CONSIDER using mapScalars
    val vtKs = (for ((k, v) <- kWm) yield (k, conv(v))).toSeq
    FP.sequence(for ((k, vt) <- vtKs) yield for (v <- vt) yield (k, v)) match {
      case Success(m) => m.toMap
      case Failure(x) => throw new ScalarException(s"cannot form sequence of Scalars from given sequence", x)
    }
  }

  /**
    * Transform a Map of K,Any into a Map of corresponding K,Scalar
    *
    * @param kWm a map of K,Any
    * @tparam K the key type
    * @return a map of Scalars
    */
  def trySequence[K](kWm: Map[K, Any])(implicit conv: ScalarMaker): Try[Map[K, Scalar]] = for (
  // CONSIDER using mapScalars
    kVs <- FP.sequence((for ((k, v) <- kWm) yield for (z <- conv(v)) yield (k, z)).toSeq)
  ) yield kVs.toMap

  val quoted = """"([^"]*)"""".r
}

