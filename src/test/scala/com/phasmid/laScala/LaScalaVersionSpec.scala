/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  *
  * @author scalaprof
  */
class LaScalaVersionSpec extends FlatSpec with Matchers {
  behavior of "getVersion"
  it should "match" in {
    LaScalaVersion.version shouldBe "1.0.1-42"
  }
}
