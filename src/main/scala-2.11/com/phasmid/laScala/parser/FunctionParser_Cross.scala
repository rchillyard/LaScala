package com.phasmid.laScala.parser

abstract class FunctionStringParser extends BaseFunctionStringParser {

  def functionString: Parser[String] = """<function\d+>""" ^^ ""

}