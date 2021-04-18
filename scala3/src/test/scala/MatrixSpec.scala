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

end MatrixSpec
