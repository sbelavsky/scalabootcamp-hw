package com.evolution.bootcamp.basics

import com.evolution.bootcamp.basics.Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class LCMSpec extends AnyFlatSpec with TableDrivenPropertyChecks {

  private val examples = Table(
    ("first number", "second number", "lcm"),
    (21, 6, 42),
    (48, 180, 720),
    (0, 0, 0),
  )

  "LCM" should "be calculated" in {
    forEvery(examples) { (a, b, expectedLCM) =>
      lcm(a, b) shouldEqual expectedLCM
    }
  }
}
