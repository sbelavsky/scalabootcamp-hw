package com.evolution.bootcamp.basics

import com.evolution.bootcamp.basics.Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class GCDSpec extends AnyFlatSpec with TableDrivenPropertyChecks {

  private val examples = Table(
    ("first number", "second number", "gcd"),
    (48, 18, 6),
    (54, 24, 6),
    (-4, 14, 2),
    (14, -4, 2),
    (1, 0, 1),
    (0, -1, 1)

  )

  "GCD" should "be calculated" in {
    forEvery(examples) { (a, b, expectedGCD) =>
      gcd(a, b) shouldBe expectedGCD
    }
  }
}
