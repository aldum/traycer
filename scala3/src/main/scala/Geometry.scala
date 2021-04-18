package pw.aldum.traycer

import scala.collection.immutable.ArraySeq
import scala.annotation.targetName

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

  extension (vec: ArraySeq[Double])
    @targetName("scalarProduct") def *(that: ArraySeq[Double]): Double =
      (vec.zipWithIndex.map((v, i) => v * that(i))).sum

  case class Matrix( val cells: ArraySeq[ArraySeq[Double]] ):
    def nRows = this.cells.size
    def nCols = this.cells(0).size

    def this(nRows: Int, nCols: Int, v: Double = 0.0) = this(
      cells = ArraySeq.fill(nRows)(
        ArraySeq.fill(nCols)(v)
      )
    )

    def apply(row: Int) = cells(row)

    def apply(nRows: Int, nCols: Int, v: Double) =
      new Matrix(nRows, nCols, v)


    override def toString =
      s"""${nRows}x${nCols} matrix:
      |${ cells.map(row => row.mkString(" ")).mkString("\n") }
      |""".stripMargin

    def ==(that: Matrix): Boolean =
      // three cheers for parameter untupling, no `case` keywords!
      cells.zipWithIndex.forall{ (r, i) =>
        r.zipWithIndex.forall{ (c, j) =>
          c == that(i)(j)
        }
      }

    def !=(that: Matrix): Boolean = !(this == that)

    def row(r: Int): ArraySeq[Double] = cells(r)

    def col(c: Int): ArraySeq[Double] =
      cells.map(r => r(c))

    @targetName("product")
    def *(that: Matrix): Matrix =
      val result: ArraySeq[ArraySeq[Double]] =
        ArraySeq.tabulate(nRows, that.nCols)(
          (i: Int, j: Int) =>
            this(i) * that.col(j)
        )
      Matrix(result)

  end Matrix


end Geometry

