package io.bigpanda.sherlock.command.model.command.json

import com.maraney.hackassembler.Types._
import com.maraney.hackassembler.{Parser, UnitSpec}

class ParserSpec extends UnitSpec {
  "The Parser" when {
    "parsing blank lines" must {
      "parse lines with whitespace" in {
        Parser.parse("        ") shouldEqual NoOp
      }
      "parse lines with no charactesr" in {
        Parser.parse("") shouldEqual NoOp
      }
      "ignore comments" in {
        Parser.parse("     //pomegranate") shouldEqual NoOp
      }
    }
    "parsing A commands" must {
      "parse commands with literal values, including whitespace" in {
        Parser.parse("   @613    ") shouldEqual ACmd(LiteralValue(613))
      }
      "parse commands with symbolic values, including whitespace" in {
        Parser.parse("   @My_DESTINATION3    ") shouldEqual ACmd(SymbolicValue("My_DESTINATION3"))
      }
      "ignore comments" in {
        Parser.parse("  @613   //pomegranate") shouldEqual ACmd(LiteralValue(613))
      }

      "return a syntax error where there are unexpected characters" in {
        Parser.parse("   @613;") shouldEqual Malformed
        Parser.parse("   h@613") shouldEqual Malformed
      }
    }
    "parsing C commands" must {
      "handle all commands" in {
        Parser.parse("  0") shouldEqual CCmd(A, Zero, Set(), NoJump)
        Parser.parse("1  ") shouldEqual CCmd(A, One, Set(), NoJump)
        Parser.parse("-1") shouldEqual CCmd(A, NegOne, Set(), NoJump)
        Parser.parse("D") shouldEqual CCmd(A, EqD, Set(), NoJump)
        Parser.parse("A") shouldEqual CCmd(A, EqSel, Set(), NoJump)
        Parser.parse("M") shouldEqual CCmd(M, EqSel, Set(), NoJump)
        Parser.parse("!D") shouldEqual CCmd(A, NotD, Set(), NoJump)
        Parser.parse("!A") shouldEqual CCmd(A, NotSel, Set(), NoJump)
        Parser.parse("!M") shouldEqual CCmd(M, NotSel, Set(), NoJump)
        Parser.parse("-D") shouldEqual CCmd(A, NegD, Set(), NoJump)
        Parser.parse("-A") shouldEqual CCmd(A, NegSel, Set(), NoJump)
        Parser.parse("-M") shouldEqual CCmd(M, NegSel, Set(), NoJump)
        Parser.parse("D+1") shouldEqual CCmd(A, DPlusOne, Set(), NoJump)
        Parser.parse("A+1") shouldEqual CCmd(A, SelPlusOne, Set(), NoJump)
        Parser.parse("M+1") shouldEqual CCmd(M, SelPlusOne, Set(), NoJump)
        Parser.parse("D-1") shouldEqual CCmd(A, DMinusOne, Set(), NoJump)
        Parser.parse("A-1") shouldEqual CCmd(A, SelMinusOne, Set(), NoJump)
        Parser.parse("M-1") shouldEqual CCmd(M, SelMinusOne, Set(), NoJump)
        Parser.parse("D+A") shouldEqual CCmd(A, DPlusSel, Set(), NoJump)
        Parser.parse("D+M") shouldEqual CCmd(M, DPlusSel, Set(), NoJump)
        Parser.parse("D-A") shouldEqual CCmd(A, DMinusSel, Set(), NoJump)
        Parser.parse("D-M") shouldEqual CCmd(M, DMinusSel, Set(), NoJump)
        Parser.parse("A-D") shouldEqual CCmd(A, SelMinusD, Set(), NoJump)
        Parser.parse("M-D") shouldEqual CCmd(M, SelMinusD, Set(), NoJump)
        Parser.parse("D&A") shouldEqual CCmd(A, DAndSel, Set(), NoJump)
        Parser.parse("D&M") shouldEqual CCmd(M, DAndSel, Set(), NoJump)
        Parser.parse("D|A") shouldEqual CCmd(A, DOrSel, Set(), NoJump)
        Parser.parse("D|M") shouldEqual CCmd(M, DOrSel, Set(), NoJump)
      }

      "return syntax errors for unrecognised commands" in {
        Parser.parse("A&A") shouldEqual Malformed
      }

      "parse destinations" in{
        Parser.parse("   A=0    ") shouldEqual CCmd(A, Zero, Set(A), NoJump)
        Parser.parse("   DAM=D+A    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), NoJump)
        Parser.parse("   DDD=D+A    ") shouldEqual CCmd(A, DPlusSel, Set(D), NoJump)
      }

      "return syntax errors for unrecognised destinations" in {
        Parser.parse("C=0") shouldEqual Malformed
      }

      "parse jumps" in{
        Parser.parse("   DAM=D+A;JGT    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JGT)
        Parser.parse("   DAM=D+A;JEQ    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JEQ)
        Parser.parse("   DAM=D+A;JGE    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JGE)
        Parser.parse("   DAM=D+A;JLT    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JLT)
        Parser.parse("   DAM=D+A;JNE    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JNE)
        Parser.parse("   DAM=D+A;JLE    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JLE)
        Parser.parse("   DAM=D+A;JMP    ") shouldEqual CCmd(A, DPlusSel, Set(D, A, M), JMP)
      }

      "return syntax errors for unrecognised jumps" in {
        Parser.parse("0;JNN") shouldEqual Malformed
      }
    }
    "parsing labels for jumps" must {
      "parse labels, including whitespace" in {
        Parser.parse("   (Label_7)    ") shouldEqual Label("Label_7")
      }
      "ignore comments" in {
        Parser.parse("  (Label_7)   //pomegranate") shouldEqual Label("Label_7")
      }

      "return a syntax error where there are unexpected characters" in {
        Parser.parse("   @(Label_7) @") shouldEqual Malformed
        Parser.parse("   (Label_7 a) ") shouldEqual Malformed
      }
    }
  }
}
