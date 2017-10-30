package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

class SymbolCalculatorSpec extends UnitSpec {
  "The Symbol Calculator" when {
    "calculating label symbols" must {
      "correctly add labels to the Symbol Table" in {
        val startingTable = SymbolTable(
          Map(
            "Existing" -> LiteralAddress(123),
            "Values" -> LiteralAddress(456)
          ))

        val lines = List(
          Label("BEGINNING"), //0
          NoOp,
          NoOp,
          ACmd(LiteralAddress(123)), //0
          ACmd(LiteralAddress(123)), //1
          ACmd(LiteralAddress(123)), //2
          NoOp,
          NoOp,
          Label("Middle"), //3
          Label("Middle2"), //3
          ACmd(LiteralAddress(123)), //3
          Label("end") //4
        )

        val desiredTable = SymbolTable(
          Map(
            "Existing" -> LiteralAddress(123),
            "Values" -> LiteralAddress(456),
            "BEGINNING" -> LiteralAddress(0),
            "Middle" -> LiteralAddress(3),
            "Middle2" -> LiteralAddress(3),
            "end" -> LiteralAddress(4)
          ))

        SymbolCalculator.calculateLabels(lines, startingTable) shouldEqual desiredTable
      }
      "use the latest value for labels that appear twice" in {
        val startingTable = SymbolTable(
          Map(
            "Existing" -> LiteralAddress(123)
          ))

        val lines = List(
          Label("Existing"), //0
          ACmd(LiteralAddress(123)), //0
          Label("Existing") //1
        )

        val desiredTable = SymbolTable(
          Map(
            "Existing" -> LiteralAddress(1)
          ))

        SymbolCalculator.calculateLabels(lines, startingTable) shouldEqual desiredTable
      }
    }
    "calculating variables" must {
      "correctly add variables to the Symbol Table" in {
        val startingTable = SymbolTable(
          Map(
            "Existing" -> LiteralAddress(123)
          ))

        val lines = List(
          ACmd(SymbolicAddress("Existing")), //123
          ACmd(SymbolicAddress("NewTwice")), //16
          ACmd(SymbolicAddress("NewTwice")), //16
          ACmd(SymbolicAddress("NewOnce")) //17
        )

        val desiredTable = SymbolTable(
          Map(
            "Existing" -> LiteralAddress(123),
            "NewTwice" -> LiteralAddress(16),
            "NewOnce" -> LiteralAddress(17)
          ))

        SymbolCalculator.calculateVariables(lines, startingTable) shouldEqual desiredTable
      }
    }
  }
}
