/*
 * LaScala
 * Copyright (c) 2016, 2017. Phasmid Software
 */

package com.phasmid.laScala

import com.phasmid.laScala.values.CaseClasses

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

/**
  * Case class which defines a Renderable Product object.
  *
  * NOTE: this is the Scala 2 version, which uses runtime reflection to find the parameter types.
  *
  * @param t the case class to be rendered
  */
case class RenderableCaseClass[T: TypeTag](t: T) extends Renderable {

  def render(indent: Int = 0)(implicit tab: Int => Prefix): String = {
    val p = t.asInstanceOf[Product]
    val sb = new StringBuilder(s"${p.productPrefix}(")
    for (((k, _), v) <- RenderableCaseClass.getParameters(t)) sb.append(nl(indent + 1) + s"$k:" + Renderable.renderElem(v, indent + 1))
    sb.append(nl(indent + 1) + ")")
    sb.toString()
  }
}

object RenderableCaseClass {
  def getParameters[T: TypeTag](t: T): ListMap[(String, Option[universe.Type]), Any] = ListMap.from(CaseClasses.caseClassParamsOf zip t.asInstanceOf[Product].productIterator.toSeq)
}
