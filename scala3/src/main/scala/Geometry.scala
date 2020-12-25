package pw.aldum.traycer

import scala.collection.immutable.{ Vector => _ }

val epsilon = 1e17

object Geometry:
  // type Fourth = 0.0 | 1.0
  // def +(f1: Fourth, f2:Fourth): Fourth =
  //   (f1, f2) match
  //     case (1, 1) => ??? //throw new ArgumentError("")
  //     case (0, 0) => 0.0
  //     case _ => 1.0

  sealed case class Vec4D(x: Double, y: Double, z: Double, w: Double):
    override def toString: String = s"{$x, $y, $z, $w}"

    def isPoint: Boolean = w == 1
    def isVector: Boolean = w == 0

    def length: Double =
      Math.sqrt(x * x + y * y + z * z)

    def isUnit: Boolean = length == 1.0

    def normalized: Vec4D =
      Vec4D( x / length
           , y / length
           , z / length
           , w)

    def +(that: Vec4D) =
      Vec4D( x + that.x
           , y + that.y
           , z + that.z
           , w + that.w)

    def -(that: Vec4D) =
      Vec4D( x - that.x
           , y - that.y
           , z - that.z
           , w - that.w)

    def unary_- = Vec4D( -x, -y, -z, w)

    def *(sc: Double) =
      Vec4D( x * sc
           , y * sc
           , z * sc
           , w)

    def /(sc: Double) = this * (1/sc)

    def *(that: Vec4D): Double =
      x * that.x +
      y * that.y +
      z * that.z +
      w * that.w

    def x(that: Vec4D): Vec4D =
      Vector( y * that.z - z * that.y
            , z * that.x - x * that.z
            , x * that.y - y * that.x)

    def ==(that: Vec4D): Boolean =
      Seq( Math.abs(x - that.x)
         , Math.abs(y - that.y)
         , Math.abs(z - that.z)
         , Math.abs(w - that.w)
         ).forall(_ < epsilon)

  end Vec4D


  // case class Vector() extends Vec4D
  def Vector(x: Double, y: Double, z: Double) = Vec4D(x, y, z, 0.0)
  def Point(x: Double, y: Double, z: Double) = Vec4D(x, y, z, 1.0)

end Geometry

