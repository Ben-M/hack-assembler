package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

import scala.util.{Failure, Success, Try}

object Assembler {

  val predefinedSymbols = SymbolTable(
    Map(
      "SP" -> LiteralAddress(0),
      "LCL" -> LiteralAddress(1),
      "ARG" -> LiteralAddress(2),
      "THIS" -> LiteralAddress(2),
      "THAT" -> LiteralAddress(4),
      "SCREEN" -> LiteralAddress(16384),
      "KBD" -> LiteralAddress(24576),
      "R0" -> LiteralAddress(0),
      "R1" -> LiteralAddress(1),
      "R2" -> LiteralAddress(2),
      "R3" -> LiteralAddress(3),
      "R4" -> LiteralAddress(4),
      "R5" -> LiteralAddress(5),
      "R6" -> LiteralAddress(6),
      "R7" -> LiteralAddress(7),
      "R8" -> LiteralAddress(8),
      "R9" -> LiteralAddress(9),
      "R10" -> LiteralAddress(10),
      "R11" -> LiteralAddress(11),
      "R12" -> LiteralAddress(12),
      "R13" -> LiteralAddress(13),
      "R14" -> LiteralAddress(14),
      "R15" -> LiteralAddress(15)
    ))

  def assemble(lines: List[String]): Try[List[String]] = {
    val parsed = lines.map(Parser.parse)
    val syntaxError = parsed.zipWithIndex.collectFirst {
      case (Malformed, line) => SyntaxError(line + 1)
    }
    if (syntaxError.isDefined) return Failure(syntaxError.get)
    val symbolTableWithLabels =
      SymbolCalculator.calculateLabels(parsed, predefinedSymbols)
    val symbolTable =
      SymbolCalculator.calculateVariables(parsed, symbolTableWithLabels)
    val commands = parsed.collect { case cmd: Cmd => cmd }
    Success(commands.map(Coder.code(_, symbolTable)))
  }
}
