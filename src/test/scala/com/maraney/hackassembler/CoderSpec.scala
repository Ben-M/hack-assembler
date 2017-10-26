package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

class CoderSpec extends UnitSpec {
  "The Parser" when {
    "coding A commands" must {
      "output the correct code" in {
        Coder.code(ACmd(Address(613))) shouldEqual "0000001001100101"
      }
      "handle very large values by ignoring the most significant bits" in {
        Coder.code(ACmd(Address(32767))) shouldEqual "0111111111111111"
      }
    }

    "coding M commands" must {
      "provide the correct prefix" in {
        Coder.code(CCmd(A, DPlusSel, Set(D, A, M), JNE)).substring(0,3) shouldEqual "111"
        Coder.code(CCmd(M, EqSel, Set(), NoJump)).substring(0,3) shouldEqual "111"
      }
      "select the correct register as input" in {
        Coder.code(CCmd(A, EqSel, Set(), NoJump)).substring(3,4) shouldEqual "0"
        Coder.code(CCmd(M, EqSel, Set(), NoJump)).substring(3,4) shouldEqual "1"
      }
      "provide the correct command" in {
        Coder.code(CCmd(A, Zero, Set(), NoJump)).substring(4,10) shouldEqual "101010"
        Coder.code(CCmd(A, One, Set(), NoJump)).substring(4,10) shouldEqual "111111"
        Coder.code(CCmd(A, NegOne, Set(), NoJump)).substring(4,10) shouldEqual "111010"
        Coder.code(CCmd(A, EqD, Set(), NoJump)).substring(4,10) shouldEqual "001100"
        Coder.code(CCmd(A, EqSel, Set(), NoJump)).substring(4,10) shouldEqual "110000"
        Coder.code(CCmd(A, NotD, Set(), NoJump)).substring(4,10) shouldEqual "001101"
        Coder.code(CCmd(A, NotSel, Set(), NoJump)).substring(4,10) shouldEqual "110001"
        Coder.code(CCmd(A, NegD, Set(), NoJump)).substring(4,10) shouldEqual "001111"
        Coder.code(CCmd(A, NegSel, Set(), NoJump)).substring(4,10) shouldEqual "110011"
        Coder.code(CCmd(A, DPlusOne, Set(), NoJump)).substring(4,10) shouldEqual "011111"
        Coder.code(CCmd(A, SelPlusOne, Set(), NoJump)).substring(4,10) shouldEqual "110111"
        Coder.code(CCmd(A, DMinusOne, Set(), NoJump)).substring(4,10) shouldEqual "001110"
        Coder.code(CCmd(A, SelMinusOne, Set(), NoJump)).substring(4,10) shouldEqual "110010"
        Coder.code(CCmd(A, DPlusSel, Set(), NoJump)).substring(4,10) shouldEqual "000010"
        Coder.code(CCmd(A, DMinusSel, Set(), NoJump)).substring(4,10) shouldEqual "010011"
        Coder.code(CCmd(A, SelMinusD, Set(), NoJump)).substring(4,10) shouldEqual "000111"
        Coder.code(CCmd(A, DAndSel, Set(), NoJump)).substring(4,10) shouldEqual "000000"
        Coder.code(CCmd(A, DOrSel, Set(), NoJump)).substring(4,10) shouldEqual "010101"
      }
      "provide the correct jump" in {
        Coder.code(CCmd(M, EqSel, Set(), NoJump)).substring(13,16) shouldEqual "000"
        Coder.code(CCmd(M, EqSel, Set(), JGT)).substring(13,16) shouldEqual "001"
        Coder.code(CCmd(M, EqSel, Set(), JEQ)).substring(13,16) shouldEqual "010"
        Coder.code(CCmd(M, EqSel, Set(), JGE)).substring(13,16) shouldEqual "011"
        Coder.code(CCmd(M, EqSel, Set(), JLT)).substring(13,16) shouldEqual "100"
        Coder.code(CCmd(M, EqSel, Set(), JNE)).substring(13,16) shouldEqual "101"
        Coder.code(CCmd(M, EqSel, Set(), JLE)).substring(13,16) shouldEqual "110"
        Coder.code(CCmd(M, EqSel, Set(), JMP)).substring(13,16) shouldEqual "111"
      }
      "provide the correct destinations" in {
        Coder.code(CCmd(M, EqSel, Set(), NoJump)).substring(10,13) shouldEqual "000"
        Coder.code(CCmd(M, EqSel, Set(M), NoJump)).substring(10,13) shouldEqual "001"
        Coder.code(CCmd(M, EqSel, Set(D), NoJump)).substring(10,13) shouldEqual "010"
        Coder.code(CCmd(M, EqSel, Set(M, D), NoJump)).substring(10,13) shouldEqual "011"
        Coder.code(CCmd(M, EqSel, Set(A), NoJump)).substring(10,13) shouldEqual "100"
        Coder.code(CCmd(M, EqSel, Set(M, A), NoJump)).substring(10,13) shouldEqual "101"
        Coder.code(CCmd(M, EqSel, Set(D, A), NoJump)).substring(10,13) shouldEqual "110"
        Coder.code(CCmd(M, EqSel, Set(M, D, A), NoJump)).substring(10,13) shouldEqual "111"
      }
      "correctly encode full commands" in{
        Coder.code(CCmd(A, EqD, Set(), JGT)) shouldEqual "1110001100000001"
        Coder.code(CCmd(A, DPlusSel, Set(D), NoJump)) shouldEqual "1110000010010000"
        Coder.code(CCmd(M, One, Set(M, D, A), JMP)) shouldEqual "1111111111111111"
      }
    }
  }
}
