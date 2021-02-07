package com.evolution.bootcamp.controlstructures

import com.evolution.bootcamp.controlstructures.ControlStructuresHomework.Command._
import com.evolution.bootcamp.controlstructures.ControlStructuresHomework.{Output, calculate, parseCommand, renderResult}
import org.scalatest.funsuite.AnyFunSuite

class ControlStructuresHomeworkTest extends AnyFunSuite {

  test("parseCommand divide 4 5") {
    assert(parseCommand("divide 4 5") == Right(Divide(4, 5)))
  }
  test("parseCommand sum 5 5 6 8.5") {
    assert(parseCommand("sum 5 5 6 8.5") == Right(Sum(List(5, 5, 6, 8.5))))
  }
  test("parseCommand average 4 3 8.5 4") {
    assert(parseCommand("average 4 3 8.5 4") == Right(Average(List(4, 3, 8.5, 4))))
  }
  test("parseCommand min 4 -3 -17") {
    assert(parseCommand("min 4 -3 -17") == Right(Min(List(4, -3, -17))))
  }
  test("parseCommand max 4 -3 -17") {
    assert(parseCommand("max 4 -3 -17") == Right(Max(List(4, -3, -17))))
  }

  test("calculate divide of 4 by 5") {
    assert(calculate(Divide(4, 5)) == Right(Output(Divide(4, 5), 0.8)))
  }

  test("calculate sum of 5 5 6 8.5 is 24.5") {
    assert(calculate(Sum(List(5, 5, 6, 8.5))) == Right(Output(Sum(List(5, 5, 6, 8.5)), 24.5)))
  }

  test("calculate sum of empty list returns error") {
    assert(calculate(Sum(Nil)).isLeft)
  }

  test("calculate average 4 3 8.5 4 is 4.5") {
    assert(calculate(Average(List(4, 3, 8.5, 4))) == Right(Output(Average(List(4, 3, 8.5, 4)), 4.875)))
  }

  test("calculate average of empty list returns error") {
    assert(calculate(Average(Nil)).isLeft)
  }

  test("calculate min 4 -3 -17 is -17") {
    assert(calculate(Min(List(4, -3, -17))) == Right(Output(Min(List(4, -3, -17)), -17)))
  }

  test("calculate min of empty list returns error") {
    assert(calculate(Min(Nil)).isLeft)
  }

  test("render result of divide") {
    assert(renderResult(Output(Divide(4, 5), 0.8)) == "4 divided by 5 is 0.8")
  }

  test("render result of sum") {
    assert(renderResult(Output(Sum(List(5, 5, 6, 8.5)), 24.5)) == "the sum of 5 5 6 8.5 is 24.5")
  }

  test("render result of average") {
    assert(renderResult(Output(Average(List(4, 3, 8.5, 4)), 4.875)) == "the average of 4 3 8.5 4 is 4.875")
  }

  test("render result of min") {
    assert(renderResult(Output(Min(List(4, -3, -17)), -17)) == "the minimum of 4 -3 -17 is -17")
  }

  test("render result of max") {
    assert(renderResult(Output(Max(List(4, -3, -17)), 4)) == "the maximum of 4 -3 -17 is 4")
  }
}
