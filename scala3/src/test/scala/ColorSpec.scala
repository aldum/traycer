package pw.aldum.traycer

import Color._

import org.scalatest._
import flatspec.AnyFlatSpec

class ColorSpec extends AnyFlatSpec:

  "adding colors" should "result" in {
    val c1 = Color(.9, .6, .75)
    val c2 = Color(.7, .1, .25)
    assert(c1 + c2 == Color(1.6, .7, 1.0))
  }
  "subbing colors" should "result" in {
    val c1 = Color(.9, .6, .75)
    val c2 = Color(.7, .1, .25)
    assert(c1 - c2 == Color(.2, .5, .5))
  }

  "multiplying by scalar" should "result" in {
    val c = Color(.2, .3, .4)
    assert(c * 2 == Color(.4, .6, .8))
  }
  "multiplying colors" should "result" in {
    val c1 = Color(1, .2, .4)
    val c2 = Color(.9, 1, .1)
    assert(c1 * c2 == Color(.9, .2, .04))
  }

end ColorSpec
