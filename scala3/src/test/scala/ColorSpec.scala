package pw.aldum.traycer

import Color._

import org.scalatest._
import flatspec.AnyFlatSpec

class ColorSpec extends AnyFlatSpec:

  "adding colors" should "result" in {
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, 0.25)
    assert(c1 + c2 == Color(1.6, 0.7, 1.0))
  }
  "subbing colors" should "result" in {
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, 0.25)
    assert(c1 - c2 == Color(0.2, 0.5, 0.5))
  }
  /*
  Scenario: Subtracting colors
    Given c1 ← color(0.9, 0.6, 0.75)
      And c2 ← color(0.7, 0.1, 0.25)
    Then c1 - c2 = color(0.2, 0.5, 0.5)

  Scenario: Multiplying a color by a scalar
    Given c ← color(0.2, 0.3, 0.4)
    Then c * 2 = color(0.4, 0.6, 0.8)

  Scenario: Multiplying colors
    Given c1 ← color(1, 0.2, 0.4)
      And c2 ← color(0.9, 1, 0.1)
    Then c1 * c2 = color(0.9, 0.2, 0.04)
  */
end ColorSpec
