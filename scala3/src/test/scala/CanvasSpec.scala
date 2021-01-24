package pw.aldum.traycer

import Color._

import org.scalatest._
import flatspec.AnyFlatSpec

class CanvasSpec extends AnyFlatSpec:
  val c = new Canvas(10, 20)

  "creating a canvas" should "result" in {
    assert(c.width == 10)
    assert(c.height == 20)
    for {
      x <- 0 until c.width
      y <- 0 until c.height
    } yield c.pixelAt(x, y) == Color.empty
  }

  "writing pixel" should "result" in {
    val red = Color(255, 0, 0)
    val can = c.writePixel(2, 3, red)
    assert( can.pixelAt(2, 3) === red )
  }

end CanvasSpec
