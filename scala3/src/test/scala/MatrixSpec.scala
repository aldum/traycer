package pw.aldum.traycer

import org.scalatest._
import flatspec.AnyFlatSpec

import Geometry.Matrix
import scala.collection.immutable.ArraySeq

class MatrixSpec extends AnyFlatSpec:

  "4x4 matrix cons" should "happen" in {
    val m0 = new Matrix(4, 4)
    assert(m0(1)(1) == 0.0)

    val m = Matrix(
      ArraySeq( ArraySeq( 1  , 2   ,  3   ,  4   )
              , ArraySeq( 5.5, 6.5 ,  7.5 ,  8.5 )
              , ArraySeq( 9  , 10.0, 11.0 , 12.0 )
              , ArraySeq(13.5, 14.5, 15.5 , 16.5 )
    ))

    assert( m(0)(0) == 1 )
    assert( m(0)(3) == 4 )
    assert( m(1)(0) == 5.5 )
    assert( m(1)(2) == 7.5 )
    assert( m(2)(2) == 11 )
    assert( m(3)(0) == 13.5 )
    assert( m(3)(2) == 15.5 )
  }

  "2x2 matrix cons" should "happen" in {
    val m = Matrix(
      ArraySeq( ArraySeq( -3, 5 )
              , ArraySeq( 1 , -2)
    ))
    assert( m(0)(0) == -3 )
    assert( m(0)(1) == 5 )
    assert( m(1)(0) == 1 )
    assert( m(1)(1) == -2 )
  }
  "3x3 matrix cons" should "happen" in {
    val m = Matrix(
      ArraySeq( ArraySeq(-3,  5,  0)
              , ArraySeq( 1, -2, -7)
              , ArraySeq( 0,  1,  1)
    ))
    assert( m(0)(0) == -3 )
    assert( m(1)(1) == -2 )
    assert( m(2)(2) == 1 )
  }

  "matrix equality" should "test" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 1 , 2 , 3 , 4 )
              , ArraySeq( 5 , 6 , 7 , 8 )
              , ArraySeq( 9 , 8,  7 , 6 )
              , ArraySeq( 5 , 4 , 3 , 2 )
    ))
    val b = Matrix(
      ArraySeq( ArraySeq( 1 , 2 , 3 , 4 )
              , ArraySeq( 5 , 6 , 7 , 8 )
              , ArraySeq( 9 , 8,  7 , 6 )
              , ArraySeq( 5 , 4 , 3 , 2 )
    ))
    assert( a == b )
  }

  "matrix inequality" should "test" in {
    val a = Matrix(
      ArraySeq( ArraySeq( 1 , 2 , 3 , 4 )
              , ArraySeq( 5 , 6 , 7 , 8 )
              , ArraySeq( 9 , 8,  7 , 6 )
              , ArraySeq( 5 , 4 , 3 , 2 )
    ))
    val b = Matrix(
      ArraySeq( ArraySeq( 2, 3, 4, 5 )
              , ArraySeq( 6, 7, 8, 9 )
              , ArraySeq( 8, 7, 6, 5 )
              , ArraySeq( 4, 3, 2, 1 )
    ))
    assert( a != b )
  }

  "matrix column" should "result" in {
    val m = Matrix(
      ArraySeq( ArraySeq( 1 , 2 , 3 , 4 )
              , ArraySeq( 5 , 6 , 7 , 8 )
              , ArraySeq( 9 , 8,  7 , 6 )
              , ArraySeq( 5 , 4 , 3 , 2 )
    ))
    assert( m.col(1).zip(ArraySeq(2, 6, 8, 4)).forall(_ == _))
  }


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
  "matrix multiplication" should "result" in {
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
  "multiplication with identity matrix" should "not change the original" in {
    assert( t1 * i4 == Some(t1) )
  }

  "transposing symmetrical matrix" should "not change it" in {
    assert(i4.transposed == i4)
    assert(reverse.transposed == reverse)
  }

  "transposing" should "work" in {
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

  "submatrix" should "return submatrix" in {
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


end MatrixSpec
