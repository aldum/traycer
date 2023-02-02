package pw.aldum.traycer

import Geometry.*

import org.scalacheck.*

object GeometryCheck extends Properties("Geometry"):
  import Gen.*
  import Prop.forAll
  import Arbitrary.arbitrary

  val genPoint = for
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
  yield Point(x,y,z)

  val genVector = for
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
  yield Vector(x,y,z)

  // property("Point") = forAll(genPoint) { case Vec4D(_,_,_,w) => w == 1.0 }
  // property("Vector") = forAll(genVector) { case Vec4D(_,_,_,w) => w == 0.0 }


  val genPointVec = for
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
    w <- 1.0
  yield Vec4D(x,y,z,w)

  val genVectorVec = for
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
    w <- 0.0
  yield Vec4D(x,y,z,w)

  val genVectorX = for
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
    w <- arbitrary[Double]
  yield Vec4D(x,y,z,w)

  property("Vec4DP") = forAll(genPointVec) { v => v.isPoint && !v.isVector }
  property("Vec4DV") = forAll(genVectorVec) { v => v.isVector && !v.isPoint }
  property("Vec4X") = forAll(genVectorVec) { v => !(v.isVector && v.isPoint) }

  property("VectorLength") = forAll(genVectorVec) { _.length >= 0 }

  // TODO
  // property("normal length") = forAll(genVectorVec) { v =>
  //   val l = v.length
  //   if (l.isFinite && l > 0) v.normalized.isUnit else true
  //   // if (l.isFinite) v.normalized.isUnit else true
  // }

end GeometryCheck


import org.scalatest.*
import flatspec.AnyFlatSpec
class GeometrySpec extends AnyFlatSpec:
  // import flatspec._
  // import matchers._// /

  "addition" `should` "work" in {
      val p1 = Point(1, 2, 3)
      val p2 = Point(1, 0, 1)
      val v1 = Vector(0, 1, 1)
      val v2 = Vector(1, 1, 0)
      assert( p1 + v1 == Point(1, 3, 4))
      assert( v1 + v1 == Vector(0, 2, 2))
      assert( v1 + v2 == Vector(1, 2, 1))
      // adding points is stupid
      assert( p1 + p2 == Vec4D(2, 2, 4, 2))
  }

  "subtraction" `should` "operate" in {
    val p1 = Point(3, 2, 1)
    val p2 = Point(5, 6, 7)
    val v1 = Vector(3, 2, 1)
    val v2 = Vector(5, 6, 7)
    val zero = Vector(0, 0, 0)
    assert(p1 - p2 == Vector(-2, -4, -6))
    assert(p1 - v2 == Point(-2, -4, -6))
    assert(v1 - v2 == Vector(-2, -4, -6))
    assert(zero - v1 == Vector(-3, -2, -1))
    // subtracting a point from a vector is also stupid
    assert(v1 - p1 == Vec4D(0, 0, 0, -1))
  }

  "negate" `should` "change sign" in {
    val v = Vector(1, -2, 3)
    assert(-v == Vector(-1, 2, -3))
  }

  "multiplication by scala" `should` "change length" in {
    val v = Vector(1, -2, 3)
    assert(v * 3.5 == Vector(3.5, -7, 10.5))
  }

  "length" `should` "calculate magnitude (pop pop)" in {
    assert(Vector(1,0,0).length == 1)
    assert(Vector(0,1,0).length == 1)
    assert(Vector(1,1,0).length == Math.sqrt(2))
    assert(Vector(1,1,1).length == Math.sqrt(3))
    assert(Vector(1,2,3).length == Math.sqrt(14))
    assert(Vector(-1,-2,-3).length == Math.sqrt(14))
  }

  "The magnitude of a normalized vector" `should` "be 1" in {
    val v = Vector(1, 2, 3)
    assert(v.normalized.length == 1)
  }

  "vector products" `should` "be" in {
    val a = Vector(1, 2, 3)
    val b = Vector(2, 3, 4)
    assert(a * b == 20)

    assert((a x b) == Vector(-1, 2, -1))
    assert((b x a) == Vector(1, -2, 1))
  }


end GeometrySpec
