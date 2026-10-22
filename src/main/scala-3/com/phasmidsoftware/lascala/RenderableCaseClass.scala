/*
 * LaScala
 * Copyright (c) 2016, 2017. Phasmid Software
 */

package com.phasmidsoftware.lascala

import scala.collection.immutable.ListMap

/**
  * Case class which defines a Renderable Product object.
  *
  * NOTE: this is the Scala 3 version. Scala 3 has no runtime reflection, so there is no TypeTag context bound,
  * and the parameter names come from productElementNames. The parameter types are not available (always None).
  *
  * @param t the case class to be rendered
  */
case class RenderableCaseClass[T](t: T) extends Renderable {

  def render(indent: Int = 0)(implicit tab: Int => Prefix): String = {
    val p = t.asInstanceOf[Product]
    val sb = new StringBuilder(s"${p.productPrefix}(")
    for (((k, _), v) <- RenderableCaseClass.getParameters(t)) sb.append(nl(indent + 1) + s"$k:" + Renderable.renderElem(v, indent + 1))
    sb.append(nl(indent + 1) + ")")
    sb.toString()
  }
}

object RenderableCaseClass {
  def getParameters[T](t: T): ListMap[(String, Option[Nothing]), Any] = {
    val p = t.asInstanceOf[Product]
    ListMap.from(p.productElementNames.map(_ -> None) zip p.productIterator)
  }
}
