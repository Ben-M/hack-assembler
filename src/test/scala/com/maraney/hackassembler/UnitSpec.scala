package com.maraney.hackassembler

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

trait UnitSpec
    extends WordSpecLike with Matchers with OptionValues with EitherValues with Inside
    with ScalaFutures with BeforeAndAfterEach {}
