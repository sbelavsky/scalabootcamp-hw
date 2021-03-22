package com.evolution.bootcamp.testing

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnsafeCalculatorTest extends AnyWordSpec with Matchers {

  import UnsafeCalculator._

  "A dumb calculator" when {
    "divide" should {
      "return a valid value" in {
        divide(1, 2) shouldBe 0.5
      }
      "Infinity if divisor is zero" in {
        divide(1, 0) shouldBe Double.PositiveInfinity
      }
    }

    "sum" should {
      "return valid result" in {
        sum(1, 2) shouldBe 3
      }
      "return zero if both args are zeroes" in {
        sum(0, 0) shouldBe 0
      }
      "apply to one of the cool math property" in {
        sum(-1, -2) shouldBe sum(-2, -1)
      }
    }

    "extract" should {
      "return valid result" in {
        extract(1, 2) shouldBe -1
      }
      "return zero if both args are zeroes" in {
        extract(0, 0) shouldBe 0
      }
    }

    "multiply" should {
      "return valid result" in {
        multiply(1, 2) shouldBe 2
      }
      "return zero if both args are zeroes" in {
        multiply(0, 0) shouldBe 0
      }
    }
  }
}
