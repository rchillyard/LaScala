package com.phasmid.laScala.tree

import com.phasmid.laScala.fp.FP
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util._


/**
  * Created by scalaprof on 10/19/16.
  */
class FunctionalTest extends FlatSpec with Matchers {

  type NodeType = Value[AccountRecord]

  behavior of "Recursive account lookup"
  // XXX we ignore this because I have not committed the sampleTree.txt file to the repository.
  it should "work for sampleTree.txt" in {
    case object TestDetailsSample extends AbstractTestDetails("sampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(7), ws(5), ws(6))
    }
    checkTreeFromResource(TestDetailsSample, 113, 3, 64, 113, 47)
  }
  // XXX we ignore this because I have not committed the exampleTree.txt file to the repository.
  ignore should "work for exampleTree.txt" in {
    case object TestDetailsSample extends AbstractTestDetails("exampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(7), ws(5), ws(6))
    }
    checkTreeFromResource(TestDetailsSample, 82740, 3, 68850, 82740, 16297)
  }

  private def checkTreeFromResource(tester: AbstractTestDetails, size: Int, depth: Int, before: Int, iteratorSize: Int, mpttSize: Int) = {
    //    Spy.spying = true
    val aso = AccountRecordTest.readAccountData(tester)
    val checks = AccountRecordTest.checkAccountTree(size, depth, before, iteratorSize, mpttSize, aso)
    println(checks)
    checks should matchPattern { case Success((`size`,`depth`,Some(_),`before`,`iteratorSize`,`mpttSize`)) => }
  }
}



