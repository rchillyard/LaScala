package com.phasmid.laScala

import com.phasmid.laScala.values.Incrementable

import scala.util.Try

/**
  * Trait Version provides a means of labeling something such that different versions can be compared.
  *
  * Created by scalaprof on 8/2/16.
  */
trait Version[V] extends Ordering[V] {
  /**
    * @return the value (tag) of this Version
    */
  def get: V

  /**
    * Method to create a new version such that the new Version will compare as greater than this Version
    * @param level for full taxonomies, this specifies the level (branch? family? taxon?) at which we are creating a new version.
    *               For example, a software release version is typically of the form major.minor.patch or something
    *               similar.
    *               Defaults to ""
    * @return the new value, wrapped in a Try
    */
  def next(level: String = ""): Try[Version[V]]
}

abstract class IncrementableVersion[V : Incrementable](tag: V) extends Version[V] {
  def compare(x: V, y: V): Int = implicitly[Incrementable[V]].compare(x,y)

  def get: V = tag

  def nextVersion(level: String = ""): Try[V] = implicitly[Incrementable[V]].increment(tag, 1, level)
}

case class LongVersion(tag: Long) extends IncrementableVersion[Long](tag) {
  def next(level: String): Try[Version[Long]] = for (l <- nextVersion(level)) yield LongVersion(l)
}

/**
  * Trait Versioned models something which has both a value and a version
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
