package pw.aldum.traycer

import scala.collection.immutable.ArraySeq
import scala.annotation.targetName

val epsilon = 1e-5

object Geometry:
  export scala.collection.immutable.ArraySeq
  // type Fourth = 0.0 | 1.0
  // def +(f1: Fourth, f2:Fourth): Fourth =
  //   (f1, f2) match
  //     case (1, 1) => ??? //throw new ArgumentError("")
  //     case (0, 0) => 0.0
  //     case _ => 1.0

  case class Vec4D(val x: Double, val y: Double, val z: Double, val w: Double):
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

    @targetName("scalarProduct")
    def *(that: Vec4D): Double =
      x * that.x +
      y * that.y +
      z * that.z +
      w * that.w

    @targetName("crossProduct")
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

  def Vector(x: Double, y: Double, z: Double) = Vec4D(x, y, z, 0.0)
  def Point(x: Double, y: Double, z: Double) = Vec4D(x, y, z, 1.0)

  extension (vec: ArraySeq[Double])
    @targetName("scalarProduct") def *(that: ArraySeq[Double]): Double =
      (vec.zipWithIndex.map((v, i) => v * that(i))).sum

  extension [T](vec: ArraySeq[T])
    /** Return a new ArraySeq without the element at the specified index.
     *  @param at If you provide an index out of bounds, this is a noop
     */
    def splice(at: Int): ArraySeq[T] =
      if at < vec.length || at < 0
      then vec.take(at) ++ vec.takeRight(vec.length - (at + 1))
      else vec

  case class Matrix( val cells: ArraySeq[ArraySeq[Double]] ):
    def nRows = this.cells.size
    def nCols = this.cells(0).size

    def this(nRows: Int, nCols: Int, v: Double = 0.0) = this(
      cells = ArraySeq.fill(nRows)(
        ArraySeq.fill(nCols)(v)
      )
    )

    def apply(row: Int) = cells(row)
    def apply(row: Int, col: Int): Option[Double] =
      if (row > 0 && row < nRows) && (col > 0 && col < nCols)
      then Some(cells(row)(col))
      else None

    def apply(nRows: Int, nCols: Int, v: Double) =
      new Matrix(nRows, nCols, v)


    override def toString =
      val cellsText = cells.map(row =>
          row.map(e => "%.5f".format(e)
        ).mkString(" ")
      ).mkString("\n")
      s"""|${nRows}x${nCols} matrix:
      |${cellsText}
      |""".stripMargin

    def ==(that: Matrix): Boolean =
      // three cheers for parameter untupling, no `case` keywords!
      cells.zipWithIndex.forall{ (r, i) =>
        r.zipWithIndex.forall{ (c, j) =>
          val d = Math.abs(c - that(i)(j))
          d <= epsilon
        }
      }

    def !=(that: Matrix): Boolean = !(this == that)

    def row(r: Int): ArraySeq[Double] = cells(r)

    def col(c: Int): ArraySeq[Double] = cells.map(r => r(c))

    @targetName("product")
    def *(that: Matrix): Option[Matrix] =
      if nCols != that.nRows then None
      else
        val result: ArraySeq[ArraySeq[Double]] =
          ArraySeq.tabulate(nRows, that.nCols)(
            (i: Int, j: Int) =>
              this(i) * that.col(j)
          )
        Some(Matrix(result))

    def transposed: Matrix =
      val result: ArraySeq[ArraySeq[Double]] =
        ArraySeq.tabulate(nCols, nRows)(
          (i: Int, j: Int) =>
            this(j)(i)
        )
      Matrix(result)

    def submatrix(row: Int, col: Int): Matrix =
      val subCells = cells
        .splice(row)
        .map(_.splice(col))
      Matrix(subCells)

    lazy val determinant: Option[Double] =
      (nRows, nCols) match
        case (r, c) if r != c => None
        case (1, 1) => Some(this(0)(0))
        case (2, 2) => Some(this(0)(0) * this(1)(1) - this(0)(1) * this(1)(0))
        case _ =>
          val firstRow = cells(0)
          val coeffs = for c <- 0 until nCols
                       yield firstRow(c) * unsafeCofactor(0, c)
          Some(coeffs.sum)

    private lazy val unsafeDeterminant = determinant.get

    def minor(row: Int, col: Int): Option[Double] =
      submatrix(row, col).determinant

    def cofactor(row: Int, col: Int): Option[Double] =
      val sign = if (row + col) % 2 == 0 then 1 else -1
      minor(row, col).map(_ * sign)

    // If we know it's square
    private def unsafeCofactor(row: Int, col: Int): Double =
      cofactor(row, col).get

    lazy val isInvertible: Boolean = determinant.exists(_ != 0)

    def inverse: Option[Matrix] =
      if isInvertible
      then
        val result = Matrix(ArraySeq.tabulate(nCols, nRows)(
          (i: Int, j: Int) =>
            val c = unsafeCofactor(j, i) // builtin transpose
            c / unsafeDeterminant
        ))
        Some(result)
      else None

  end Matrix

  case object Matrix:
    def identity(n: Int) =
      new Matrix(cells = ArraySeq.tabulate[Double](n, n)(
          (i: Int, j: Int) =>
            if i == j then 1 else 0
        ))

  end Matrix


end Geometry

