package com.phasmid.laScala.fp

import java.io.Closeable

import com.phasmid.laScala.parser.rpn.RPN
import org.scalatest.{FlatSpec, Matchers, WordSpec}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success}

/**
  * TryWithSpec based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  * Created by scalaprof on 8/5/16.
  */
class LoggableSpec extends FlatSpec with Matchers {
  "Loggable" should "work" in {
    val loggable = LoggableSlf4jInfo(LoggerFactory.getLogger(getClass))
    loggable.invoke("99".toInt) shouldBe 99
  }
  it should "leave the proper trail" in {
    var elephantMemory = ""
    def elephantLogger(s: String) = {
      elephantMemory = elephantMemory + s + "\n"
    }
    val loggable = LoggableBasic(elephantLogger)
    loggable.invoke("99".toInt, "99") shouldBe 99
    elephantMemory shouldBe "starting 99\nfinished 99\n"
  }
}
