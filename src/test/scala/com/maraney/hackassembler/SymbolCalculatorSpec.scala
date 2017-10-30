package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

class SymbolCalculatorSpec extends UnitSpec {
  "The Symbol Calculator" when {
    "calculating label symbols" must {
      "correctly add labels to the Symbol Table" in {
        val startingTable = SymbolTable(
          Map(
            "Existing" -> LiteralValue(123),
            "Values" -> LiteralValue(456)
          ))

        val lines = List(
          Label("BEGINNING"), //0
          NoOp,
          NoOp,
          ACmd(LiteralValue(123)), //0
          ACmd(LiteralValue(123)), //1
          ACmd(LiteralValue(123)), //2
          NoOp,
          NoOp,
          Label("Middle"), //3
          Label("Middle2"), //3
          ACmd(LiteralValue(123)), //3
          Label("end") //4
        )

        val desiredTable = SymbolTable(
          Map(
            "Existing" -> LiteralValue(123),
            "Values" -> LiteralValue(456),
            "BEGINNING" -> LiteralValue(0),
            "Middle" -> LiteralValue(3),
            "Middle2" -> LiteralValue(3),
            "end" -> LiteralValue(4)
          ))

        SymbolCalculator.calculateLabels(lines, startingTable) shouldEqual desiredTable
      }
      "use the latest value for labels that appear twice" in {
        val startingTable = SymbolTable(
          Map(
            "Existing" -> LiteralValue(123)
          ))

        val lines = List(
          Label("Existing"), //0
          ACmd(LiteralValue(123)), //0
          Label("Existing") //1
        )

        val desiredTable = SymbolTable(
          Map(
            "Existing" -> LiteralValue(1)
          ))

        SymbolCalculator.calculateLabels(lines, startingTable) shouldEqual desiredTable
      }
    }
    "calculating variables" must {
      "correctly add variables to the Symbol Table" in {
        val startingTable = SymbolTable(
          Map(
            "Existing" -> LiteralValue(123)
          ))

        val lines = List(
          ACmd(SymbolicValue("Existing")), //123
          ACmd(SymbolicValue("NewTwice")), //16
          ACmd(SymbolicValue("NewTwice")), //16
          ACmd(SymbolicValue("NewOnce")) //17
        )

        val desiredTable = SymbolTable(
          Map(
            "Existing" -> LiteralValue(123),
            "NewTwice" -> LiteralValue(16),
            "NewOnce" -> LiteralValue(17)
          ))

        SymbolCalculator.calculateVariables(lines, startingTable) shouldEqual desiredTable
      }
    }
  }
}
