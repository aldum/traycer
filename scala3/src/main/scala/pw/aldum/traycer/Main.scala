package pw.aldum.traycer

import Geometry.*

object Main extends App:
  // println(msg)
  // val p = Point(1,2,3)
  // val v = Vector(1,2,3)
  // println(p)
  // println(v)
  val c1 = Color(1.5, 0, 0)
  val c2 = Color(0, .5, 0)
  val c3 = Color(-0.5, 0, 1)
  val can = new Canvas(5, 3)
          .writePixel(0, 0, c1)
          .writePixel(2, 1, c2)
          .writePixel(4, 2, c3)
  println(can.PPM.toPPM)
  // println(new Canvas(100, 100).PPM.toPPM)


