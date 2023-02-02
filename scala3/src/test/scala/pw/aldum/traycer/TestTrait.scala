package pw.aldum.traycer

import scala.language.adhocExtensions

import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

export org.scalacheck.{ Arbitrary, Gen }
export org.scalatest.compatible.Assertion

trait TestTrait
    extends AnyFunSuite,
      should.Matchers,
      GivenWhenThen,
      BeforeAndAfterAll,
      BeforeAndAfterEach,
      ScalaCheckPropertyChecks
