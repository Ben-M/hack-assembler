package com.maraney.hackassembler

import java.io._

import com.maraney.hackassembler.Types.SyntaxError

import scala.io.Source
import scala.util.{Failure, Success}

object App {
  def main(args: Array[String]) {
    args.headOption match {
      case None =>
        println(
          "Usage: \"hackrun Reassembler <name>\" will assemble <name>.asm to <name>.hack")
      case Some(filename) =>
        val lines = readFile(filename)
        Assembler.assemble(lines) match {
          case Success(assembled) => writeFile(filename, assembled)
          case Failure(SyntaxError(line)) =>
            System.err.println("Syntax error on line " + line)
          case Failure(error) =>
            System.err.println("Unrecognised error:" + error)
        }
    }
  }

  private def readFile(filename: String) = {
    Source.fromFile(filename + ".asm").getLines.toList
  }

  private def writeFile(filename: String, lines: List[String]) = {
    val writer = new FileWriter(filename + ".hack")
    for (line <- lines) writer.write(line + "\n")
    writer.close
  }
}
