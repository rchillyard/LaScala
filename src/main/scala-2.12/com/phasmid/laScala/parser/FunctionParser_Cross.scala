package com.phasmid.laScala.parser


abstract class FunctionStringParser extends BaseFunctionStringParser {

  def functionString: Parser[String] = extendedPath <~ """Lambda$""" <~ decimalNumber <~ "/" <~ decimalNumber <~ "@" <~ hexNumber

  def extendedPath: Parser[String] = """[a-zA-Z0-9_.-]+\$+""".r

  def hexNumber: Parser[String] = """[a-fA-F0-9]{1,8}""".r

}

object FunctionParser_Cross