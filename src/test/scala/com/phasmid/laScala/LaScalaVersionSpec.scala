/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

import org.scalatest.{FlatSpec, Matchers}

/**
  * See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  *
  * @author scalaprof
  */
@deprecated
class LaScalaVersionSpec extends FlatSpec with Matchers {
  behavior of "getVersion"
  ignore should "match" in {
    LaScalaVersion.version shouldBe "1.0.1-51"
  }
}
