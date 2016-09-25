package com.phasmid.laScala

import com.phasmid.laScala.values.Incrementable

import scala.util.{Failure, Success, Try}

/**
  * Trait Version provides a means of labeling something such that different versions can be compared.
  *
  * Created by scalaprof on 8/2/16.
  */
trait Version[V] extends Ordering[V] {

  /**
    * @return the sub-version of this Version, if available
    */
  def subversion: Option[Version[V]]

  /**
    * @return the value (tag) of this Version
    */
  def get: V

  /**
    * Method to create a new version such that the new Version will compare as greater than this Version
    *
    * @return the new value, wrapped in a Try
    */
  def next: Try[Version[V]]

  /**
    * Create a new Version based on this Version with a new subversion
    *
    * @param tag the tag of the new subversion (call by name which allows for the possibility of an exception being thrown
    *            while evaluating the tag -- this will result in a Failure rather than an exception.
    */
  def withSubversion(tag: => V): Try[Version[V]]
}

abstract class IncrementableVersion[V : Incrementable](tag: V) extends Version[V] {
  private val incrementable = implicitly[Incrementable[V]]

  def compare(x: V, y: V): Int = {
    val cf = incrementable.compare(x, y)
    if (cf!=0) cf
    else subversion match {
      case None => cf
      case Some(z) => incrementable.compare(z.get,y)
    }
  }

  def get: V = tag

  def nextVersion: Try[V] = incrementable.increment(tag)

  def render: String = {
    def inner(v: Version[V], s: String): String = v.subversion match {
      case None => s
      case Some(z) => inner(z,s"$s.$get")
    }
    inner(this,s"$get")
  }
}

case class LongVersion(tag: Long, subversion: Option[Version[Long]]) extends IncrementableVersion[Long](tag) {
  def next: Try[Version[Long]] = for (l <- nextVersion) yield LongVersion(l,subversion)
  def withSubversion(tag: => Long): Try[Version[Long]] = this match {
    case LongVersion(t,None) => Try(LongVersion(t,Some(LongVersion(tag))))
    case _ => Failure(new VersionException(s"version $this already has subversion"))
  }
  override def toString = render
}

object LongVersion {
  def apply(tag: Long): Version[Long] = apply(tag,None)
  def parse(s: String): Option[Version[Long]] = Version.parse(s,{_.toInt},LongVersion.apply)
}

object Version {
  def parse[V](s: String, f: String=>V, g: V=>Version[V]): Option[Version[V]] = {
    def inner(xs: List[String], vo: Option[Version[V]]): Option[Version[V]] = xs match {
      case h :: t => inner(t,
        vo match {
          case Some(v) => (for (z <- v.withSubversion(f(h))) yield z).toOption
          case None => Try(g(f(h))).toOption
        })
      case Nil => vo
    }
    inner(s.split("""\.""").toList,None)
  }
}
/**
  * Trait Versioned models something which has both a value and a version
  *
  * @tparam T the underlying type of this Versioned object
  * @tparam V the underlying type of this object's version
  */
trait Versioned[T, V] {

  /**
    * @return the value of this Versioned object
    */
  def get: T

  /**
    * @return the version of this Versioned object
    */
  def version: Version[V]
}

case class VersionException(s: String) extends Exception(s)
