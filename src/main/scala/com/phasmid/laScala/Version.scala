package com.phasmid.laScala

import java.io.File
import java.time.{Instant, LocalDateTime, ZoneId, ZoneOffset}

import lascalabuildinfo.BuildInfo
import com.phasmid.laScala.values.Incrementable

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Try}

/**
  * Trait Version provides a means of labeling something such that different versions can be compared.
  * As in the real world, a Version can consist of just one V tag: or a Version can (optionally) be extended by another Version.
  * This allows for versions of the form 1.1.2 or A.B.B. You cannot currently mix these V types.
  *
  * Created by scalaprof on 8/2/16.
  */
trait Version[V] extends Ordering[V] with Versionable[V] {

  /**
    * @return the value (tag) of this Version
    */
  def get: V

  /**
    * @return the sub-version of this Version, if available
    */
  def subversion: Option[Version[V]]

  /**
    * @return a Seq of V objects representing the version/subversion of this instance.
    */
  def toSeq: Seq[V] = {
    def inner(v: Version[V], vs: Seq[V]): Seq[V] = v.subversion match {
      case None => vs
      case Some(z) => inner(z, get +: vs)
    }

    inner(this, Seq(get))
  }

  /**
    * Create a new Version based on this Version with a new subversion
    *
    * @param tag the tag of the new subversion (call by name which allows for the possibility of an exception being thrown
    *            while evaluating the tag -- this will result in a Failure rather than an exception.
    */
  def withSubversion(tag: => V): Try[Version[V]]

  def render: String = toSeq.mkString(".")

  /**
    * This method is required to build a Version object from the given V tag and an optional subversion.
    *
    * @param v          the tag for this version
    * @param subversion optional subversion
    * @return a new Version object
    */
  def build(v: V, subversion: Option[Version[V]]): Version[V]
}

/**
  * This trait models something that is versionable, that is to say has a method next which returns a Try[Versionable[V]...
  *
  * @tparam V the underlying type of this Versionable
  */
trait Versionable[V] {
  /**
    * Method to create a new version such that the new Version will compare as greater than this Version
    *
    * @return the new value, wrapped in a Try
    */
  def next: Try[Versionable[V]]
}

abstract class IncrementableVersion[V: Incrementable](tag: V) extends Version[V] {
  private val incrementable = Incrementable[V]

  def next: Try[Version[V]] = for (l <- nextVersion) yield build(l, subversion)

  def withSubversion(v: => V): Try[Version[V]] = subversion match {
    case None => Try(build(tag, Some(build(v, None))))
    case _ => Failure(VersionException(s"version $this already has subversion"))
  }

  def compare(x: V, y: V): Int = {
    val cf = incrementable.compare(x, y)
    if (cf != 0) cf
    else subversion match {
      case None => cf
      case Some(z) => incrementable.compare(z.get, y)
    }
  }

  def get: V = tag

  def nextVersion: Try[V] = incrementable.increment(tag)

  override def toString: String = render
}

case class LongVersion(tag: Long, subversion: Option[Version[Long]]) extends IncrementableVersion[Long](tag) {
  def build(v: Long, subversion: Option[Version[Long]]): Version[Long] = new LongVersion(v, subversion)
}

object LongVersion {
  def apply(tag: Long): Version[Long] = apply(tag, None)

  def parse(s: String): Option[Version[Long]] = Version.parse(s, _.toLong, LongVersion.apply)
}

object Version {

  val versionR: Regex = """s?[^"]*"([^"]+)".*""".r
  lazy val version = Source.fromFile("version.sbt").mkString.trim match {
    case versionR(v) => v
    case _ => "<unknown version>"
  }

  /**
    * This code relies on the sbt-buildinfo plugin which creates a source file called BuildInfo.scala in the following directory:
    * target/scala-2.x/src_managed/main/sbt-buildinfo/
    */
  def getVersion : String = s"${BuildInfo.name} ${BuildInfo.version} (compiled with ${BuildInfo.scalaVersion} at $buildDateTime)"

  private def buildDateTime: String = s"${LocalDateTime.ofInstant(Instant.ofEpochMilli(BuildInfo.buildTime), ZoneOffset.UTC)} UTC"

  def parse[V](s: String, f: String => V, g: V => Version[V]): Option[Version[V]] = {
    def inner(xs: List[String], vo: Option[Version[V]]): Option[Version[V]] = xs match {
      case h :: t => inner(t,
        vo match {
          // TODO use FP.toOption
          case Some(v) => (for (z <- v.withSubversion(f(h))) yield z).toOption
          // TODO use FP.toOption
          case None => Try(g(f(h))).toOption
        })
      case Nil => vo
    }

    inner(s.split("""\.""").toList, None)
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
