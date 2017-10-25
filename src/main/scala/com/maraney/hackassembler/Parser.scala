package com.maraney.hackassembler

import com.maraney.hackassembler.Types.{Register, _}

object Parser {
  def parse(line: String) = {

    val stripWhiteSpaceAndComments = raw"\s*([^/\s]*)\s*(//.*)?".r
    val aCmd = raw"(@)(\d*)".r
    val cCmd = raw"([DAM]*\=)?([01\-DAM!&+}|]{1,3})(;[A-Z]{3})?".r

    line match {
      case stripWhiteSpaceAndComments(command, _) =>
        command match {
          case aCmd(_, address) => ACmd(Address(address.toShort))
          case cCmd(destinationsPart, computationPart, jumpPart) =>
            (for {
              (computation, selected) <- getComputationAndSelected(computationPart)
              jump <- getJump(jumpPart)
              destinations = getDestinations(destinationsPart)
            } yield CCmd(selected, computation, destinations, jump)).getOrElse(SyntaxError)
          case "" => NoOp
          case _  => SyntaxError
        }
    }
  }

  private def getDestinations(string: String): Set[Register] = {
    if (string == null) return Set()
    val withD = if (string contains "D") Set(D) else Set()
    val withA = if (string contains "A") Set(A) else Set()
    val withM = if (string contains "M") Set(M) else Set()
    withD ++ withA ++ withM
  }

  private def getComputationAndSelected(s: String) = {
    val zero = raw"0".r
    val one = raw"1".r
    val negOne = raw"-1".r
    val eqD = raw"D".r
    val eqSel = raw"([A|M])".r
    val notD = raw"!D".r
    val notSel = raw"!([A|M])".r
    val negD = raw"-D".r
    val negSel = raw"-([A|M])".r
    val dPlusOne = raw"D\+1".r
    val selPlusOne = raw"([A|M])\+1".r
    val dMinusOne = raw"D-1".r
    val selMinusOne = raw"([A|M])-1".r
    val dPlusSel = raw"D\+([A|M])".r
    val dMinusSel = raw"D-([A|M])".r
    val selMinusD = raw"([A|M])-D".r
    val dAndSel = raw"D&([A|M])".r
    val dOrSel = raw"D\|([A|M])".r

    s match {
      case zero()           => Some(Zero, A)
      case one()            => Some(One, A)
      case negOne()         => Some(NegOne, A)
      case eqD()            => Some(EqD, A)
      case eqSel(sel)       => Some(EqSel, getSelected(sel))
      case notD()           => Some(NotD, A)
      case notSel(sel)      => Some(NotSel, getSelected(sel))
      case negD()           => Some(NegD, A)
      case negSel(sel)      => Some(NegSel, getSelected(sel))
      case dPlusOne()       => Some(DPlusOne, A)
      case selPlusOne(sel)  => Some(SelPlusOne, getSelected(sel))
      case dMinusOne()      => Some(DMinusOne, A)
      case selMinusOne(sel) => Some(SelMinusOne, getSelected(sel))
      case dPlusSel(sel)    => Some(DPlusSel, getSelected(sel))
      case dMinusSel(sel)   => Some(DMinusSel, getSelected(sel))
      case selMinusD(sel)   => Some(SelMinusD, getSelected(sel))
      case dAndSel(sel)     => Some(DAndSel, getSelected(sel))
      case dOrSel(sel)      => Some(DOrSel, getSelected(sel))
      case _                => None
    }
  }

  private def getSelected(s: String) = s match {
    case "A" => A
    case "M" => M
  }

  private def getJump(s: String): Option[Jump] = {
    if (s == null) return Some(NoJump)
    s.substring(1) match {
      case null  => Some(NoJump)
      case "JGT" => Some(JGT)
      case "JEQ" => Some(JEQ)
      case "JGE" => Some(JGE)
      case "JLT" => Some(JLT)
      case "JNE" => Some(JNE)
      case "JLE" => Some(JLE)
      case "JMP" => Some(JMP)
      case _     => None
    }
  }
}
