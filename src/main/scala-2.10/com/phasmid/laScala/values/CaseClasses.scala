/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.values

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._

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
  def caseClassParamsOf[T: TypeTag]: ListMap[String, Option[Type]] =
    throw new Exception("cannot use caseClassParamsOf with Scala 2.10")
}
