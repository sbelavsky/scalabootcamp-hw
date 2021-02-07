package com.evolution.bootcamp.controlstructures

import java.text.DecimalFormat

import com.evolution.bootcamp.controlstructures.ControlStructuresHomework.Command._

import scala.io.Source

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

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    for (
      str <- Source.stdin.getLines() map { line =>
        process(line)
      }
    ) println(str)

  def process(x: String): String = {
    val eitherResult = for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield renderResult(result)

    eitherResult match {
      case Left(value) => value.toString
      case Right(value) => value
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.trim.split("\\s+").toList match {
      case "divide" :: a :: b :: Nil => Right(Divide(a.toDouble, b.toDouble))
      case "sum" :: xs => Right(Sum(xs.map(_.toDouble)))
      case "average" :: xs => Right(Average(xs.map(_.toDouble)))
      case "min" :: xs => Right(Min(xs.map(_.toDouble)))
      case "max" :: xs => Right(Max(xs.map(_.toDouble)))
      case _ => Left(ErrorMessage("unknown command"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] =
    x match {
      case Divide(_, 0) => Left(ErrorMessage("division by zero"))
      case c@Divide(x, y) => Right(Output(c, x / y))
      case Sum(Nil) => Left(ErrorMessage("empty list of numbers"))
      case c@Sum(list) => Right(Output(c, list.sum))
      case Average(Nil) => Left(ErrorMessage("empty list of numbers"))
      case c@Average(list) => Right(Output(c, list.sum / list.length))
      case Min(Nil) => Left(ErrorMessage("empty list of numbers"))
      case c@Min(list) => Right(Output(c, list.min))
      case Max(Nil) => Left(ErrorMessage("empty list of numbers"))
      case c@Max(list) => Right(Output(c, list.max))
      case _ => Left(ErrorMessage("unknown command"))
    }

  def renderResult(x: Result): String = {
    implicit val defaultFormatter: DecimalFormat = new DecimalFormat()
    defaultFormatter.setMinimumFractionDigits(0)
    x match {
      case o@Output(command, _) => command match {
        case Divide(dividend, divisor) => s"${defaultFormatter.format(dividend)} divided by ${defaultFormatter.format(divisor)} is ${o.formatted}"
        case Sum(numbers) => s"the sum of ${numbers.map(defaultFormatter.format).mkString(" ")} is ${o.formatted}"
        case Average(numbers) => s"the average of ${numbers.map(defaultFormatter.format).mkString(" ")} is ${o.formatted}"
        case Min(numbers) => s"the minimum of ${numbers.map(defaultFormatter.format).mkString(" ")} is ${o.formatted}"
        case Max(numbers) => s"the maximum of ${numbers.map(defaultFormatter.format).mkString(" ")} is ${o.formatted}"
      }
    }
  }

  sealed trait Command

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result {
    def formatted(implicit format: DecimalFormat): String
  }

  final case class ErrorMessage(value: String) {
    override def toString: String = "Error: " + value
  }

  final case class Output(command: Command, result: Double) extends Result {
    override def formatted(implicit format: DecimalFormat): String = format.format(result)
  }

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

}