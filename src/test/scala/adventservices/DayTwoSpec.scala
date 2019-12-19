package adventservices

import adventservices.DayTwo.Operation
import org.scalatest.{MustMatchers, WordSpec}

class DayTwoSpec extends WordSpec with MustMatchers {

  val oob = -1 //out of bounds

  "Operation model" must {
    "build the add command" in {
      val result = Operation(List(1, oob, oob, oob))
      result.command.map(_(4,5)) mustBe Some(9)
    }

    "build the multiply command" in {
      val result = Operation(List(2, oob, oob, oob))
      result.command.map(_(4,5)) mustBe Some(20)
    }

    "build the halt command" in {
      val result = Operation(List(99, oob, oob, oob))
      result.command mustBe None
    }
  }

  "runOperation" must {
    "run the halt operation" in {
      val code = List(99, 0, 0, 0)
      DayTwo.runOperation(Operation(code), code) mustBe Left(List(99,0,0,0))
    }

    "run the multiply operation" in {
      val code = List(2, 2, 2, 3)
      DayTwo.runOperation(Operation(code), code) mustBe Right(List(2,2,2,4))
    }

    "run the add operation" in {
      val code = List(1, 0, 0, 0)
      val operation = Operation(code)
      operation mustBe Operation(operation.command, 0, 0, 0)
      DayTwo.runOperation(operation, code) mustBe Right(List(2,0,0,0))
    }
  }

  "runProgram" must {
    "run two sequences of operations" in {
      val code = List(1, 0, 0, 0, 1, 0, 0, 0)
      DayTwo.runProgram(code) mustBe List(4, 0, 0, 0, 1, 0, 0, 0)
    }

    "run one sequence of operations because of halt" in {
      val code = List(99, 0, 0, 0, 1, 0, 0, 0)
      DayTwo.runProgram(code) mustBe List(99, 0, 0, 0, 1, 0, 0, 0)
    }

    "run some program" in {
      val code = List(1, 0, 0, 0, 1, 0, 0, 0, 99, 0, 0, 0, 1, 0, 0, 0)
      DayTwo.runProgram(code) mustBe List(4, 0, 0, 0, 1, 0, 0, 0, 99, 0, 0, 0, 1, 0, 0, 0)
    }
  }

  "run1202ProgramAlarm FIXED" in {
    DayTwo.run1202ProgramAlarm mustBe 4138658
  }
}
