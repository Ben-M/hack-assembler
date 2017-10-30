package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

object Coder {

  def code(command: Cmd, symbolTable: SymbolTable): String =
    command match {
      case ACmd(LiteralAddress(address)) => codeA(address)
      case ACmd(SymbolicAddress(label)) => {
        val address: LiteralAddress = symbolTable.table.getOrElse(label, LiteralAddress(0))
        codeA(address.address)
      }
      case CCmd(sel, cmp, dst, jmp) => "111"  + codeSel(sel) + codeCmp(cmp) + codeDst(dst) + codeJmp(jmp)
    }

  private def codeA(address: Short) =
    "0" + ("00000000000000000000" + address.toBinaryString takeRight 15)

  private def codeSel(sel: SelectableRegister) = sel match {
    case A => "0"
    case M => "1"
  }

  private def codeCmp(cmp: Computation) = cmp match {
    case Zero => "101010"
    case One => "111111"
    case NegOne => "111010"
    case EqD => "001100"
    case EqSel => "110000"
    case NotD => "001101"
    case NotSel => "110001"
    case NegD => "001111"
    case NegSel => "110011"
    case DPlusOne => "011111"
    case SelPlusOne => "110111"
    case DMinusOne => "001110"
    case SelMinusOne => "110010"
    case DPlusSel => "000010"
    case DMinusSel => "010011"
    case SelMinusD => "000111"
    case DAndSel => "000000"
    case DOrSel => "010101"
  }

  private def codeDst(dsts: Set[Register]) = {
    val d1 = if (dsts.contains(A)) "1" else "0"
    val d2 = if (dsts.contains(D)) "1" else "0"
    val d3 = if (dsts.contains(M)) "1" else "0"
    d1 + d2 + d3
  }
  private def codeJmp(jmp: Jump) = jmp match {
    case NoJump => "000"
    case JGT => "001"
    case JEQ => "010"
    case JGE => "011"
    case JLT => "100"
    case JNE => "101"
    case JLE => "110"
    case JMP => "111"
  }
}
