package com.phasmid.laScala.fp

import java.net.URL

import com.phasmid.laScala.fp.FP._
import org.scalatest._
import org.scalatest.concurrent._

import scala.util._

/**
  *
  * @author scalaprof
  */
class FP_CrossSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  // TODO need to do more thorough testing here
  // NOTE: map2lazy is only available with 2.11
  "map2lazy" should "succeed" in {
    val one = Success(1)
    val two = Success(2)
    def sum(x: Int, y: Int) = x + y

    implicit val continue: Int => Boolean = _ => true
    map2lazy(one, two)(sum) should matchPattern { case Success(3) => }
    map2lazy(one, Failure(new Exception("bad")))(sum) should matchPattern { case Failure(_) => }
  }
}
