package com.maraney.hackassembler

import com.maraney.hackassembler.Types._

import scala.io.Source
import scala.util.{Failure, Success}

class AssemblerSpec extends UnitSpec {
  "The Assembler" must {
    "assembles symbol free programs" in {
      val input =
        Source.fromFile("src/test/fixtures/symbolfree.asm").getLines.toList
      val desiredOutput: Seq[String] =
        Source.fromFile("src/test/fixtures/symbolfree.hack").getLines.toList
      val output = Assembler.assemble(input)

      output shouldEqual Success(desiredOutput)
    }

    "identifies syntax errors" in {
      val input =
        Source.fromFile("src/test/fixtures/error-line-10.asm").getLines.toList
      val output = Assembler.assemble(input)
      output shouldEqual Failure(SyntaxError(10))
    }
  }
}
