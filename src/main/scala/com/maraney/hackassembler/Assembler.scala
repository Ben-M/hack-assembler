package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

import scala.util.{Failure, Success, Try}

object Assembler {

  val predefinedSymbols = SymbolTable(
    Map(
      "SP" -> LiteralValue(0),
      "LCL" -> LiteralValue(1),
      "ARG" -> LiteralValue(2),
      "THIS" -> LiteralValue(3),
      "THAT" -> LiteralValue(4),
      "SCREEN" -> LiteralValue(16384),
      "KBD" -> LiteralValue(24576),
      "R0" -> LiteralValue(0),
      "R1" -> LiteralValue(1),
      "R2" -> LiteralValue(2),
      "R3" -> LiteralValue(3),
      "R4" -> LiteralValue(4),
      "R5" -> LiteralValue(5),
      "R6" -> LiteralValue(6),
      "R7" -> LiteralValue(7),
      "R8" -> LiteralValue(8),
      "R9" -> LiteralValue(9),
      "R10" -> LiteralValue(10),
      "R11" -> LiteralValue(11),
      "R12" -> LiteralValue(12),
      "R13" -> LiteralValue(13),
      "R14" -> LiteralValue(14),
      "R15" -> LiteralValue(15)
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
