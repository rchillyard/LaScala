/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala

import com.typesafe.scalalogging.Logging
import org.slf4j.LoggerFactory

/**
  * Created by scalaprof on 4/20/17.
  */
object LaScalaVersion {
  val logger = LoggerFactory.getLogger("LaScalaVersion")

  val version = "1.0.1-42"

  logger.debug(s"LaScalaVersion: $version")

//  private val major = 1
//  private val minor = 0
//  private val point = 1
//  private val snapshot = 42
//  val version: Version[Long] = LongVersion.parse("1.0.1.42").get
////    LongVersion(major,Some(LongVersion(minor,Some(LongVersion(point,Some(LongVersion(snapshot,None)))))))
//  def getVersion = version.toString

}
