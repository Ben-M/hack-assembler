package com.maraney.hackassembler

object Types {
  sealed trait Value
  case class LiteralValue(value: Short) extends Value
  case class SymbolicValue(symbol: String) extends Value

  sealed trait ParseResult

  sealed trait Cmd extends ParseResult
  case class ACmd(addr: Value) extends Cmd
  case class CCmd(sel: SelectableRegister,
                  cmp: Computation,
                  dst: Set[Register],
                  jmp: Jump)
      extends Cmd

  sealed trait SelectableRegister
  sealed trait Register
  object D extends Register
  object A extends Register with SelectableRegister
  object M extends Register with SelectableRegister

  sealed trait Jump
  object NoJump extends Jump
  object JGT extends Jump
  object JEQ extends Jump
  object JGE extends Jump
  object JLT extends Jump
  object JNE extends Jump
  object JLE extends Jump
  object JMP extends Jump

  sealed trait Computation
  object Zero extends Computation
  object One extends Computation
  object NegOne extends Computation
  object EqD extends Computation
  object EqSel extends Computation
  object NotD extends Computation
  object NotSel extends Computation
  object NegD extends Computation
  object NegSel extends Computation
  object DPlusOne extends Computation
  object SelPlusOne extends Computation
  object DMinusOne extends Computation
  object SelMinusOne extends Computation
  object DPlusSel extends Computation
  object DMinusSel extends Computation
  object SelMinusD extends Computation
  object DAndSel extends Computation
  object DOrSel extends Computation

  case class Label(symbol: String) extends ParseResult
  object Malformed extends ParseResult
  object NoOp extends ParseResult

  case class SyntaxError(line: Integer) extends Exception("", None.orNull)

  case class SymbolTable(table: Map[String, LiteralValue] = Map()) extends AnyVal
}