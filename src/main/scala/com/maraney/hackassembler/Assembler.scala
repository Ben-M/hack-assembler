package com.maraney.hackassembler

import com.maraney.hackassembler.Types.{Cmd, Malformed, SyntaxError}

import scala.util.{Failure, Success, Try}

object Assembler {
  def assemble(lines: List[String]): Try[List[String]] = {
    val parsed = lines.map(Parser.parse)
    val syntaxError = parsed.zipWithIndex.collectFirst {
      case (Malformed, line) => SyntaxError(line + 1)
    }
    if (syntaxError.isDefined) return Failure(syntaxError.get)
    val commands = parsed.collect { case cmd: Cmd => cmd }
    Success(commands.map(Coder.code))
  }
}
