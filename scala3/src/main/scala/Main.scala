package pw.aldum.traycer

import Geometry._
object Main:
  def msg = "I was compiled by dotty :)"

  def main(args: Array[String]): Unit =
    println(msg)
    val p = Point(1,2,3)
    val v = Vector(1,2,3)
    println(p)
    println(v)
  end main

