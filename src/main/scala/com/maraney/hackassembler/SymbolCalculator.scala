package com.maraney.hackassembler

import com.maraney.hackassembler.Types.{Cmd, _}

object SymbolCalculator {
  case class LabelState(lineNo: Short, symbolTable: SymbolTable)

  def calculateLabels(lines: List[ParseResult],
                      symbolTable: SymbolTable): SymbolTable = {
    lines
      .foldLeft(LabelState(0, symbolTable)) { (lState, parseResult) =>
        parseResult match {
          case c: Cmd =>
            lState.copy(lineNo = (lState.lineNo + 1).toShort)
          case Label(name) =>
            lState.copy(
              symbolTable = SymbolTable(lState.symbolTable.table ++ Map(
                name -> LiteralValue(lState.lineNo))))
          case _ => lState
        }
      }
      .symbolTable
  }

  case class VariableState(nextAddress: Short, symbolTable: SymbolTable)

  def calculateVariables(lines: List[ParseResult],
                         symbolTable: SymbolTable): SymbolTable = {

    lines
      .foldLeft(VariableState(16, symbolTable)) { (vState, parseResult) =>
        parseResult match {
          case ACmd(SymbolicValue(symbol)) =>
            if (vState.symbolTable.table.isDefinedAt(symbol)) {
              vState
            } else {
              val newVariable =
                Map(symbol -> LiteralValue(vState.nextAddress))
              val newSymbolTable =
                SymbolTable(vState.symbolTable.table ++ newVariable)
              VariableState((vState.nextAddress + 1).toShort, newSymbolTable)
            }
          case _ => vState
        }
      }
      .symbolTable
  }
}
