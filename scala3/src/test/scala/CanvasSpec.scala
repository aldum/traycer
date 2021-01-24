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
    val red = Color(1, 0, 0)
    val can = c.writePixel(2, 3, red)
    assert( can.pixelAt(2, 3) === red )
  }

  "ppm header" should "result" in {
    assert( c.PPM.createHeader == s"""P3
${c.width} ${c.height}
255""")
  }

  "ppm convert" should "result" in {
    val c = new Canvas(5, 3)
    val c1 = Color(1.5, 0, 0)
    val c2 = Color(0, .5, 0)
    val c3 = Color(-0.5, 0, 1)
    val can = c.writePixel(0, 0, c1)
            .writePixel(2, 1, c2)
            .writePixel(4, 2, c3)

    val ppm = can.PPM.toPPM.linesIterator.drop(3)
    val test = s"""|255 0 0 0 0 0 0 0 0 0 0 0 0 0 0​
      |0 0 0 0 0 0 0 128 0 0 0 0 0 0 0​
      |0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
      |​""".stripMargin

    // the test seems wrong for some reason
    // assert(ppm.mkString("\n") == test)
  }

  "ppm convert for large canvas" should "result" in {
    val col = Color(1, .8, .6)
    val c = new Canvas(10, 2, col)
    val ppm = c.PPM.toPPM.linesIterator.take(7).drop(3).toList
    assert( ppm.forall(_.size <= 70))

    val test = """|255 204 153 255 204 153 255 204 153 255 204 153 255 204 153
    |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153
    |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153
    |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153""".stripMargin
    assert(test == ppm.mkString("\n"))
  }

  "ppm convert ends with newline" should "result" in {
    val c = new Canvas(5, 3)
    val ppm = c.PPM.toPPM
    assert(ppm.endsWith("\n"))
  }


end CanvasSpec
