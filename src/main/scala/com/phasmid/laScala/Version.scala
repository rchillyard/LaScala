package com.phasmid.laScala

import java.time.{Instant, LocalDateTime, ZoneOffset}

import com.phasmid.laScala.fp.FP
import com.phasmid.laScala.values.Incrementable
import lascalabuildinfo.BuildInfo

import scala.util.{Success, Try}

/**
  * Case class Version provides a means of labeling something such that different versions can be compared.
  * As in the real world, a Version can consist of just one V tag: or a Version can (optionally) be extended by another Version.
  * This allows for versions of the form 1.1.2 or A.B.B. You cannot currently mix these V types.
  *
  * Created by scalaprof on 8/2/16.
  * Updated by scalaprof on 7/11/17.
  */
case class Version[V: Incrementable](v: V, subversion: Option[Subversioned[V]], isSnapshot: Boolean = false) extends BaseVersion[V, Version[V]](v) with Sequential[Version[V]] with Subversionable[Version[V]] {
  def build(v: V, vso: Option[Subversioned[V]], isSnapshot: Boolean): Version[V] = Version(v, vso, isSnapshot)

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append(v)
    subversion match {
      case Some(x) => sb.append(s".$x")
      case _ =>
    }
    if (isSnapshot) sb.append(" (snapshot)")
    sb.toString()
  }
}

/**
  * Trait which defines a Stream of subversions based on this instance of Subversionable.
  *
  * @tparam T the underlying type
  */
trait Subversionable[T] {
  /**
    * Method to create a Stream of Ts based on this instance of Subversionable.
    *
    * @return a Stream of Ts.
    */
  def subversions: Stream[T]
}

/**
  * Trait which represents a version value (V) which can be sub-versioned.
  * Instances of this trait can be ordered so that they can be compared.
  *
  * @tparam V the underlying type of this Subversioned object.
  *           Typically, this will be a String or an integral value such as Long.
  */
trait Subversioned[V] extends (() => V) with Ordering[Subversioned[V]] {

  /**
    * Method to get the subversion of this Subversioned, if any.
    *
    * @return Some(vv) or None
    */
  def subversion: Option[Subversioned[V]]

  /**
    * Method to get this Subversioned and all its subversions, as a Stream of V values.
    *
    * @return a Stream of V
    */
  def asStream: Stream[V] = {
    Stream.cons(apply(),
      subversion match {
        case Some(vv) => vv.asStream
        case None => Stream.empty[V]
      })
  }
}

/**
  * Abstract class which extends Subversioned with Ordering.
  * It defines the following behavior:
  * (1) the apply method yields the value of v;
  * (2) the compare method yields the comparison of this and that.
  *
  * @param v the value of this Subversioned
  * @tparam V the underlying type of the Subversioned, which is defined to implement Ordering
  */
abstract class BaseVersion[V: Incrementable, Repr](v: V) extends Subversioned[V] with OldRenderable {

  def build(v: V, vso: Option[Subversioned[V]], isSnapshot: Boolean): Repr

  def next(isSnapshot: Boolean = false): Try[Repr] = for (x <- implicitly[Incrementable[V]].increment(v)) yield build(x, None, isSnapshot = false)

  def subversions: Stream[Version[V]] = {
    def s(v: V): Stream[V] = implicitly[Incrementable[V]].increment(v) match {
      case Success(w) => Stream.cons(v, s(w))
      case _ => Stream.cons(v, Stream.empty)
    }

    s(implicitly[Incrementable[V]].zero) map { z => Version(v, Some(Version(z, None))) }
  }

  def apply(): V = v

  /**
    * @return a Seq of V objects representing the version/subversions of this instance.
    *         The value of isSnapshot is ignored.
    */
  def toSeq: Seq[V] = {
    def inner(result: Seq[V], version: Subversioned[V]): Seq[V] = version match {
      case Version(x, Some(y), _) => inner(result :+ x, y)
      case Version(x, _, _) => result :+ x
    }

    inner(Seq(), this)
  }

  def render(indent: Int)(implicit tab: (Int) => Prefix): String = toSeq.mkString(".")

  def compare(x: Subversioned[V], y: Subversioned[V]): Int = implicitly[Ordering[V]].compare(x(), y())

  protected val cf: (Subversioned[V], Subversioned[V]) => Int = compare

  def compare(that: Subversioned[V]): Int = FP.map2(this.subversion, that.subversion)(cf).getOrElse(if (subversion.isDefined) -1 else 1)
}

object Version {

  /**
    * This code relies on the sbt-buildinfo plugin which creates a source file called BuildInfo.scala in the following directory:
    * target/scala-2.x/src_managed/main/sbt-buildinfo/
    */
  def getVersion: String = s"${BuildInfo.name} ${BuildInfo.version} (compiled with ${BuildInfo.scalaVersion} at $buildDateTime)"

  private def buildDateTime: String = s"${LocalDateTime.ofInstant(Instant.ofEpochMilli(BuildInfo.buildTime), ZoneOffset.UTC)} UTC"

  def apply(x: Long): Version[Long] = Version(x, None)

  def longVersion(s: String): Version[Long] = apply(s.toLong)

  def parseLong(s: String): Option[Version[Long]] = parse(s)(longVersion)

  def parse[V](s: String)(f: String => Version[V]): Option[Version[V]] = {
    def inner(vo: Option[Version[V]], w: String, xs: List[String]): Option[Version[V]] = xs match {
      case h :: t =>
        val sb = new StringBuilder()
        if (w.nonEmpty) sb.append(s"$w.")
        sb.append(h)
        val ss = sb.toString()
        inner(vo match {
          case Some(v) => v.subversions.find(x => x.toString() == ss)
          case None => Try(f(h)).toOption
        }, ss, t)
      case Nil => vo
    }

    inner(None, "", s.split("""\.""").toList)
  }
}

/**
  * Trait Versioned models something which has a version
  *
  * @tparam V the underlying type of this object's version
  */
trait Versioned[V] {

  /**
    * @return the version of this Versioned object
    */
  def version: Version[V]
}

case class VersionException(s: String) extends Exception(s)
