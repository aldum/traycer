package pw.aldum.traycer

import org.scalatest.*
import flatspec.AnyFlatSpec

import Geometry.Matrix
import scala.collection.immutable.ArraySeq

class MatrixOpsSpec extends AnyFlatSpec:

  val t1 = Matrix(
    ArraySeq( ArraySeq( 1, 2, 3, 4 )
            , ArraySeq( 2, 4, 4, 2 )
            , ArraySeq( 8, 6, 4, 1 )
            , ArraySeq( 0, 0, 0, 1 )
  ))
  val reverse = Matrix(
    ArraySeq( ArraySeq( 0, 1 )
            , ArraySeq( 1, 0 )
  ))
  "matrix multiplication" `should` "result" in {
    val twoByFour = Matrix(
      ArraySeq( ArraySeq( 1, 2, 3, 4 )
              , ArraySeq( 2, 4, 4, 2 )
    ))
    val r2b4 = Matrix(
      ArraySeq( ArraySeq( 2, 4, 4, 2 )
              , ArraySeq( 1, 2, 3, 4 )
    ))
    assert( reverse * twoByFour == Some(r2b4) )

    val t2 = Matrix(
      ArraySeq( ArraySeq( 1 )
              , ArraySeq( 2 )
              , ArraySeq( 3 )
              , ArraySeq( 1 )
    ))
    val r2 = Matrix(
      ArraySeq( ArraySeq( 18 )
              , ArraySeq( 24 )
              , ArraySeq( 33 )
              , ArraySeq( 1 )
    ))
    val m2 = t1 * t2
    assert( m2 == Some(r2) )

    val h1 = Matrix(
      ArraySeq( ArraySeq( 1, 2, 3, 4 )
              , ArraySeq( 2, 4, 4, 2 )
              , ArraySeq( 8, 6, 4, 1 )
    ))
    val hr = Matrix(
      ArraySeq( ArraySeq( 18 )
              , ArraySeq( 24 )
              , ArraySeq( 33 )
    ))
    assert( h1 * t2 == Some(hr) )

    val v2 = Matrix( ArraySeq(ArraySeq( 2 , 4, 4, 2 )) )
    val s = Matrix(ArraySeq(ArraySeq(24)))
    assert(v2 * t2 == Some(s))

    // book
    val a = Matrix(
      ArraySeq( ArraySeq( 1, 2, 3, 4 )
              , ArraySeq( 5, 6, 7, 8 )
              , ArraySeq( 9, 8, 7, 6 )
              , ArraySeq( 5, 4, 3, 2 )
    ))
    val b = Matrix(
      ArraySeq( ArraySeq( -2, 1, 2, 3 )
              , ArraySeq( 3 , 2, 1, -1 )
              , ArraySeq( 4 , 3, 6, 5 )
              , ArraySeq( 1 , 2, 7, 8 )
    ))
    val r = Matrix(
      ArraySeq( ArraySeq(20, 22,  50,  48)
              , ArraySeq(44, 54, 114, 108)
              , ArraySeq(40, 58, 110, 102)
              , ArraySeq(16, 26,  46,  42)
    ))
    val m = a * b
    assert( m == Some(r) )
  }

  val i4 = Matrix.identity(4)
  "multiplication with identity matrix" `should` "not change the original" in {
    assert( t1 * i4 == Some(t1) )
  }

  "transposing symmetrical matrix" `should` "not change it" in {
    assert(i4.transposed == i4)
    assert(reverse.transposed == reverse)
  }

  "transposing" `should` "work" in {
    val tbt = Matrix(
      ArraySeq( ArraySeq( 0, 9, 3, 0 )
              , ArraySeq( 9, 8, 0, 8 )
              , ArraySeq( 1, 8, 5, 3 )
              , ArraySeq( 0, 0, 5, 8 )
    ))
    val tt = Matrix(
      ArraySeq( ArraySeq( 0, 9, 1, 0 )
              , ArraySeq( 9, 8, 8, 0 )
              , ArraySeq( 3, 0, 5, 5 )
              , ArraySeq( 0, 8, 3, 8 )
    ))
    assert(tbt.transposed == tt)
  }

  "submatrix" `should` "return submatrix" in {
    val m = Matrix(
      ArraySeq( ArraySeq( 1 , 5, 0 )
              , ArraySeq( -3, 2, 7 )
              , ArraySeq( 0 , 6, -3 )
    ))
    val sm = Matrix(
      ArraySeq( ArraySeq( -3, 2 )
              , ArraySeq( 0 , 6 )
    ))
    assert(m.submatrix(0, 2) == sm)
  }

  "determinant of 2x2" `should` "calculate correctly" in {
    val m1 = Matrix(
      ArraySeq( ArraySeq( 1 , 5 )
              , ArraySeq( -3, 2 )
    ))
    assert(m1.determinant == Some(17))
  }

  "minor" `should` "calculate correctly" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 3 , 5, 0 )
              , ArraySeq( 2, -1, -7 )
              , ArraySeq( 6 , -1, 5 )
    ))
    val b = a.submatrix(1, 0)
    assert(b.determinant == Some(25))
    assert(a.minor(1, 0) == Some(25))
  }

  "cofactor" `should` "calculate correctly" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 3 , 5, 0 )
              , ArraySeq( 2, -1, -7 )
              , ArraySeq( 6 , -1, 5 )
    ))
    assert(a.minor(0, 0) == Some(-12))
    assert(a.cofactor(0, 0) == Some(-12))
    assert(a.minor(1, 0) == Some(25))
    assert(a.cofactor(1, 0) == Some(-25))
  }

  "determinant of 3x3" `should` "calculate correctly" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 1 , 2, 6  )
              , ArraySeq( -5, 8, -4 )
              , ArraySeq( 2 , 6, 4  )
    ))
    assert(a.cofactor(0, 0) == Some(56))
    assert(a.cofactor(0, 1) == Some(12))
    assert(a.cofactor(0, 2) == Some(-46))
    assert(a.determinant == Some(-196))
  }

  "determinant of 4x4" `should` "calculate correctly" in {
    val a = Matrix(
      ArraySeq( ArraySeq( -2, -8, 3 , 5 )
              , ArraySeq( -3, 1 , 7 , 3 )
              , ArraySeq( 1 , 2 , -9, 6 )
              , ArraySeq( -6, 7 , 7 ,-9 )
    ))
    assert(a.cofactor(0, 0) == Some(690))
    assert(a.cofactor(0, 1) == Some(447))
    assert(a.cofactor(0, 2) == Some(210))
    assert(a.cofactor(0, 3) == Some(51))
    assert(a.determinant == Some(-4071))
  }

  "invertibility" `should` "be determined" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 6, 4, 4, 4 )
              , ArraySeq( 5, 5, 7, 6 )
              , ArraySeq( 4,-9, 3,-7 )
              , ArraySeq( 9, 1, 7,-6 )
    ))
    assert(a.determinant == Some(-2120))
    assert(a.isInvertible == true)
    val b = Matrix(
      ArraySeq( ArraySeq(-4, 2,-2,-3 )
              , ArraySeq( 9, 6, 2, 6 )
              , ArraySeq( 0,-5, 1,-5 )
              , ArraySeq( 0, 0, 0, 0 )
    ))
    assert(b.determinant == Some(0))
    assert(b.isInvertible == false)
  }

  "inverting" `should` "work" in {
    val a = Matrix(
      ArraySeq( ArraySeq(-5 ,  2 ,  6 , -8 )
              , ArraySeq( 1 , -5 ,  1 ,  8 )
              , ArraySeq( 7 ,  7 , -6 , -7 )
              , ArraySeq( 1 , -3 ,  7 ,  4 )
    ))
    assert(a.determinant == Some(532))
    assert(a.isInvertible == true)
    assert(a.cofactor(2, 3) == Some(-160))
    val b = a.inverse.get
    val ai = Matrix(
      ArraySeq( ArraySeq( 0.218045,  0.45113 ,  0.24060 , -0.04511)
              , ArraySeq(-0.80827 , -1.45677 , -0.44361 ,  0.52068)
              , ArraySeq(-0.07895 , -0.22368 , -0.05263 ,  0.19737)
              , ArraySeq(-0.52256 , -0.81391 , -0.30075 ,  0.30639)
    ))
    assert(b == ai)

    val a2 = Matrix(
      ArraySeq( ArraySeq( 8 , -5 ,  9 ,  2 )
              , ArraySeq( 7 ,  5 ,  6 ,  1 )
              , ArraySeq(-6 ,  0 ,  9 ,  6 )
              , ArraySeq(-3 ,  0 , -9 , -4 )
    ))
    val a2i = Matrix(
      ArraySeq( ArraySeq( -0.15385 , -0.15385 , -0.28205 , -0.53846 )
              , ArraySeq( -0.07692 ,  0.12308 ,  0.02564 ,  0.03077 )
              , ArraySeq(  0.35897 ,  0.35897 ,  0.43590 ,  0.92308 )
              , ArraySeq( -0.69231 , -0.69231 , -0.76923 , -1.92308 )
    ))
    assert( a2.inverse.get == a2i )

    val a3 = Matrix(
      ArraySeq( ArraySeq(  9 ,  3 ,  0 ,  9 )
              , ArraySeq( -5 , -2 , -6 , -3 )
              , ArraySeq( -4 ,  9 ,  6 ,  4 )
              , ArraySeq( -7 ,  6 ,  6 ,  2 )
    ))
    val a3i = Matrix(
      ArraySeq( ArraySeq( -0.04074 , -0.07778 ,  0.14444 , -0.22222 )
              , ArraySeq( -0.07778 ,  0.03333 ,  0.36667 , -0.33333 )
              , ArraySeq( -0.02901 , -0.14630 , -0.10926 ,  0.12963 )
              , ArraySeq(  0.17778 ,  0.06667 , -0.26667 ,  0.33333 )
    ))
    assert( a3.inverse.get == a3i )
  }

  "multiplying by inverse" `should` "reproduce the original" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 3 , -9 ,  7 ,  3 )
              , ArraySeq( 3 , -8 ,  2 , -9 )
              , ArraySeq(-4 ,  4 ,  4 ,  1 )
              , ArraySeq(-6 ,  5 , -1 ,  1 )
    ))
    val b = Matrix(
      ArraySeq( ArraySeq(  8 ,  2 ,  2 ,  2 )
              , ArraySeq(  3 , -1 ,  7 ,  0 )
              , ArraySeq(  7 ,  0 ,  5 ,  4 )
              , ArraySeq(  6 , -2 ,  0 ,  5 )
    ))
    val c = (a * b).get
    val bi = b.inverse.get
    assert((c * bi).get == a)
  }

end MatrixOpsSpec
