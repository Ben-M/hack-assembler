package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

class CoderSpec extends UnitSpec {
  "The Parser" when {
    "coding A commands" must {
      "output the correct code for literal values" in {
        Coder.code(ACmd(LiteralAddress(613)), SymbolTable()) shouldEqual "0000001001100101"
      }
      "handle very large literal values by ignoring the most significant bits" in {
        Coder.code(ACmd(LiteralAddress(32767)), SymbolTable()) shouldEqual "0111111111111111"
      }
      "output the correct code from the symbol table for  symbolic values" in {
        val table = SymbolTable(
          Map(
            "RIGHT" -> LiteralAddress(613),
            "WRONG" -> LiteralAddress(123)
          ))
        Coder.code(ACmd(SymbolicAddress("RIGHT")), table) shouldEqual "0000001001100101"
      }
    }

    "coding M commands" must {
      "provide the correct prefix" in {
        Coder.code(CCmd(A, DPlusSel, Set(D, A, M), JNE), SymbolTable()).substring(0,3) shouldEqual "111"
        Coder.code(CCmd(M, EqSel, Set(), NoJump), SymbolTable()).substring(0,3) shouldEqual "111"
      }
      "select the correct register as input" in {
        Coder.code(CCmd(A, EqSel, Set(), NoJump), SymbolTable()).substring(3,4) shouldEqual "0"
        Coder.code(CCmd(M, EqSel, Set(), NoJump), SymbolTable()).substring(3,4) shouldEqual "1"
      }
      "provide the correct command" in {
        Coder.code(CCmd(A, Zero, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "101010"
        Coder.code(CCmd(A, One, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "111111"
        Coder.code(CCmd(A, NegOne, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "111010"
        Coder.code(CCmd(A, EqD, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "001100"
        Coder.code(CCmd(A, EqSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "110000"
        Coder.code(CCmd(A, NotD, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "001101"
        Coder.code(CCmd(A, NotSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "110001"
        Coder.code(CCmd(A, NegD, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "001111"
        Coder.code(CCmd(A, NegSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "110011"
        Coder.code(CCmd(A, DPlusOne, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "011111"
        Coder.code(CCmd(A, SelPlusOne, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "110111"
        Coder.code(CCmd(A, DMinusOne, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "001110"
        Coder.code(CCmd(A, SelMinusOne, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "110010"
        Coder.code(CCmd(A, DPlusSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "000010"
        Coder.code(CCmd(A, DMinusSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "010011"
        Coder.code(CCmd(A, SelMinusD, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "000111"
        Coder.code(CCmd(A, DAndSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "000000"
        Coder.code(CCmd(A, DOrSel, Set(), NoJump), SymbolTable()).substring(4,10) shouldEqual "010101"
      }
      "provide the correct jump" in {
        Coder.code(CCmd(M, EqSel, Set(), NoJump), SymbolTable()).substring(13,16) shouldEqual "000"
        Coder.code(CCmd(M, EqSel, Set(), JGT), SymbolTable()).substring(13,16) shouldEqual "001"
        Coder.code(CCmd(M, EqSel, Set(), JEQ), SymbolTable()).substring(13,16) shouldEqual "010"
        Coder.code(CCmd(M, EqSel, Set(), JGE), SymbolTable()).substring(13,16) shouldEqual "011"
        Coder.code(CCmd(M, EqSel, Set(), JLT), SymbolTable()).substring(13,16) shouldEqual "100"
        Coder.code(CCmd(M, EqSel, Set(), JNE), SymbolTable()).substring(13,16) shouldEqual "101"
        Coder.code(CCmd(M, EqSel, Set(), JLE), SymbolTable()).substring(13,16) shouldEqual "110"
        Coder.code(CCmd(M, EqSel, Set(), JMP), SymbolTable()).substring(13,16) shouldEqual "111"
      }
      "provide the correct destinations" in {
        Coder.code(CCmd(M, EqSel, Set(), NoJump), SymbolTable()).substring(10,13) shouldEqual "000"
        Coder.code(CCmd(M, EqSel, Set(M), NoJump), SymbolTable()).substring(10,13) shouldEqual "001"
        Coder.code(CCmd(M, EqSel, Set(D), NoJump), SymbolTable()).substring(10,13) shouldEqual "010"
        Coder.code(CCmd(M, EqSel, Set(M, D), NoJump), SymbolTable()).substring(10,13) shouldEqual "011"
        Coder.code(CCmd(M, EqSel, Set(A), NoJump), SymbolTable()).substring(10,13) shouldEqual "100"
        Coder.code(CCmd(M, EqSel, Set(M, A), NoJump), SymbolTable()).substring(10,13) shouldEqual "101"
        Coder.code(CCmd(M, EqSel, Set(D, A), NoJump), SymbolTable()).substring(10,13) shouldEqual "110"
        Coder.code(CCmd(M, EqSel, Set(M, D, A), NoJump), SymbolTable()).substring(10,13) shouldEqual "111"
      }
      "correctly encode full commands" in{
        Coder.code(CCmd(A, EqD, Set(), JGT), SymbolTable()) shouldEqual "1110001100000001"
        Coder.code(CCmd(A, DPlusSel, Set(D), NoJump), SymbolTable()) shouldEqual "1110000010010000"
        Coder.code(CCmd(M, One, Set(M, D, A), JMP), SymbolTable()) shouldEqual "1111111111111111"
      }
    }
  }
}
