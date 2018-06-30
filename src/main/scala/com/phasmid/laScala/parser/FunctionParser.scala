package com.phasmid.laScala.parser

import scala.util._
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

abstract class BaseFunctionStringParser extends JavaTokenParsers {

  def parseCompoundFunctionString(s: String): Try[(String, Seq[String], String)] =
    parseAll(functionStringParser, s) match {
      case this.Success(p, _) => scala.util.Success(p)
      case this.Failure(x, _) => FunctionParser.failure(s, x, "compound function string")
      case this.Error(x, _) => FunctionParser.failure(s, x, "compound function string")
    }

  def parseFunctionString(s: String): Try[String] =
    parseAll(functionString, s) match {
      case this.Success(p, _) => scala.util.Success(p)
      case this.Failure(x, _) => FunctionParser.failure(s, x, "function string")
      case this.Error(x, _) => FunctionParser.failure(s, x, "function string")
    }

  def functionStringParser: Parser[(String, List[String], String)] = prefix ~ repsep(functionString, word) ~ suffix ^^ {
    case p ~ fs ~ s => (p, fs, s)
  }

  def word: Parser[String] = FunctionParser.word

  def functionString: Parser[String]

  def prefix: Parser[String]

  def suffix: Parser[String]
}

object FunctionParser {
  val word: Regex = """\s*\w+\s*""".r

  def failure(s: String, x: String, z: String) = {
    scala.util.Failure(new Exception(
      s"""failed to parse "$s" as a $z: $x""".stripMargin))
  }

}