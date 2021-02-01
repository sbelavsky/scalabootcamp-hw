package com.evolution.bootcamp.basics

import scala.annotation.tailrec

object Basics {
  def lcm(a: Int, b: Int): Int = if (a ==0 || b == 0) 0 else math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    (math.abs(a), math.abs(b)) match {
      case (x, y) if x == 0 => y
      case (x, y) if y == 0 => x
      case (x, y) if x > y => gcd(x % y, y)
      case (x, y) => gcd(x, y % x)
    }
  }
}