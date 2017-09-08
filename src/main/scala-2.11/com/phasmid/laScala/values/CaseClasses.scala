/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe.{Type, TypeTag, termNames, typeOf}
import scala.util.Try

object CaseClasses {
  /**
    * The following method is used by RenderableCaseClass
    *
    * Returns a map from formal parameter names to types, containing one
    * mapping for each constructor argument.  The resulting map (a ListMap)
    * preserves the order of the primary constructor's parameter list.
    *
    * Provenance: https://stackoverflow.com/questions/16079113/scala-2-10-reflection-how-do-i-extract-the-field-values-from-a-case-class/16079804#16079804
    */
  def caseClassParamsOf[T: TypeTag]: ListMap[String, Option[Type]] = {
    val tpe = typeOf[T]
    val constructorSymbol = tpe.decl(termNames.CONSTRUCTOR)
    val defaultConstructor =
      if (constructorSymbol.isMethod) constructorSymbol.asMethod
      else {
        val ctors = constructorSymbol.asTerm.alternatives
        ctors.map(_.asMethod).find(_.isPrimaryConstructor).get
      }

    ListMap[String, Option[Type]]() ++ defaultConstructor.paramLists.reduceLeft(_ ++ _).map {
      sym => sym.name.toString -> Try(tpe.member(sym.name).asMethod.returnType).toOption
    }
  }
}
