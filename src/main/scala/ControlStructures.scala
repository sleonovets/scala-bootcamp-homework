package com.evolutiongaming.bootcamp.basics

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  object Operation extends Enumeration {
    val DIVIDE: Operation.Value = Value("divide")
    val SUM: Operation.Value = Value("sum")
    val AVERAGE: Operation.Value = Value("average")
    val MIN: Operation.Value = Value("min")
    val MAX: Operation.Value = Value("max")
  }

  final case class ErrorMessage(value: String)

  sealed trait Result

  final case class CommandResult(op: Operation.Value, value: Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    val parameters = x.trim.split("\\s+") toList

    val parsed = parameters.tail.map(el => Try(el.toDouble) match {
      case Success(value: Double) => value
      case Failure(exception) => exception.toString
    })

    //    val numbers: List[Double] = for (a: Double <- parsed) yield a
    val numbers: List[Double] = parsed.collect { case a: Double => a }


    if (parsed.length != numbers.length) {
      return Left(ErrorMessage("Error: numbers parse error"))
    }

    if (numbers.length < 2) {
      return Left(ErrorMessage("Error: should be three arguments at least"))
    }

    Try(Operation.withName(parameters.head)) match {
      case Success(operation) => operation match {
        case Operation.DIVIDE => Right(Command.Divide(numbers.head, numbers.tail.head))
        case Operation.SUM => Right(Command.Sum(numbers))
        case Operation.AVERAGE => Right(Command.Average(numbers))
        case Operation.MAX => Right(Command.Max(numbers))
        case Operation.MIN => Right(Command.Min(numbers))
      }
      case Failure(_) => Left(ErrorMessage("Error: command type is not correct"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(dividend, divisor) => divisor match {
        case 0 => Left(ErrorMessage("Error: invalid operation"))
        case _ => Right(CommandResult(Operation.DIVIDE, dividend / divisor))
      }
      case Command.Sum(numbers) => Right(CommandResult(Operation.SUM, numbers.sum))
      case Command.Min(numbers) => Right(CommandResult(Operation.MIN, numbers.min))
      case Command.Max(numbers) => Right(CommandResult(Operation.MAX, numbers.max))
      case Command.Average(numbers) => numbers.length match {
        case 0 => Left(ErrorMessage("Error: invalid operation"))
        case _ => Right(CommandResult(Operation.AVERAGE, numbers.sum / numbers.length))
      }
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case CommandResult(operation: Operation.Value, value) => s"the result of the ${operation} command is ${value.toString}"
      case _ => "Error: unexpected error"
    }
  }

  def process(x: String): String = {

    val evaluationResult = for {
      parsedCommand <- parseCommand(x)
      result <- calculate(parsedCommand)
    } yield renderResult(result)

    evaluationResult.left.map(_.value).merge
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
