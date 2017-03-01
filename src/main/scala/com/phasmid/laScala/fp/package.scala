/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

/**
  * Created by scalaprof on 11/12/16.
  */
package object fp {
  type Parameter[T] = Either[T, Closure[_, T]]
}
