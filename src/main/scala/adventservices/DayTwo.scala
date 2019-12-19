package adventservices

import util.InputReader

object DayTwo {

  def runOperation(op: Operation, programList: List[Int]): Either[List[Int], List[Int]] =
    op.command.fold[Either[List[Int], List[Int]]](
      Left(programList)
    )(command =>
      Right(programList.replaceAtIndexWith(op.posResult, command(programList(op.pos1), programList(op.pos2))))
    )


  def runProgram(program: List[Int]): List[Int] ={
    val operations = program.sliding(4, 4).toList
    operations
      .map(Operation(_))
      .takeWhile(_.command.isDefined)
      .foldLeft(program)((prog, operation) =>
          runOperation(operation, prog).fold(identity, identity)
      )
  }

  def fixProgram(program: List[Int]) ={
    program
      .replaceAtIndexWith(1, 12)
      .replaceAtIndexWith(2, 2)
  }

  def run1202ProgramAlarm(): Int = {
    val program = util.InputReader.getInput("day_2_input").mkString("").split(",").toList.map(_.toInt)
    val fixedProgram = fixProgram(program)
    runProgram(fixedProgram).head
  }

  final case class Operation(command: Option[(Int,Int) => Int], pos1: Int, pos2: Int, posResult: Int)

  object Operation {
    def apply(list: List[Int]): Operation = {
      val pos1 = list.takeRight(3).head
      val pos2 = list.takeRight(2).head
      val posRes = list.takeRight(1).head
      val command = list.head match {
        case 1 => Some((a:Int,b:Int)=> a+b)
        case 2 => Some((a:Int,b:Int)=> a*b)
        case _ => None
      }

      new Operation(command, pos1, pos2, posRes)
    }
  }

  implicit class RichList[A](val list: List[A]) extends AnyVal {
    def replaceAtIndexWith(index: Int, replacement: A): List[A] = {
      if (index > list.size) {
        throw new IndexOutOfBoundsException(s"Cant replace at index $index as list length is ${list.size}")
      } else {
        val split = list.splitAt(index)
        (split._1 :+ replacement) ++ split._2.tail
      }
    }
  }

}
