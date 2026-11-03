/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala.parser

import scala.io.Source
import scala.util.Try

trait Ingestible[X] {
  def fromString(w: String): Try[X]
}

class Ingest[T: Ingestible] extends (Iterable[String] => Iterator[Try[T]]) {
  def apply(source: Iterable[String]): Iterator[Try[T]] = source.iterator.map(e => implicitly[Ingestible[T]].fromString(e))

  def apply(source: Source): Iterator[Try[T]] = apply(source.getLines().to(Iterable))
}

object Ingest {
  def ignoreHeader(source: Source, n: Int = 1): Iterable[String] = source.getLines().drop(n).to(Iterable)
}