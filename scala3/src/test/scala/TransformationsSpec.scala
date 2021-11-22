package pw.aldum.traycer

import org.scalatest.*
import flatspec.AnyFlatSpec

import Geometry.*
import Geometry.given

class TransformationsSpec extends TestTrait:
  import Transformation.*

  val p = Point(-3, 4, 5)
  val t = Translate(Vector(5, -3, 2))
  test("Multiplying by a translation matrix") {
    /*
      Given transform ← translation(5, -3, 2)
      And p ← point(-3, 4, 5)
      Then transform * p = point(2, 1, 7)
    */
    val r = Point(2, 1, 7)

    assert( t * p == colMat(r) )
  }

  test("Multiplying by the inverse of a translation matrix") {
    /*
      Given​ transform ← translation(5, -3, 2)
​   	    ​And​ inv ← inverse(transform)
​   	    ​And​ p ← point(-3, 4, 5)
​   	   ​Then​ inv * p = point(-8, 7, 3)
    */
    val tInv = t.inverse
    val r2 = Point(-8, 7, 3)

    assert( (tInv * p).get == colMat(r2) )
  }

  test("Translation does not affect vectors") {
    /*
      ​Given​ transform ← translation(5, -3, 2)
​   	    ​And​ v ← vector(-3, 4, 5)
​   	   ​Then​ transform * v = v
    */
    val v = Vector(5, -3, 2)
    assert( t * v == colMat(v) )
  }

  val s = Scale(Vector(2, 3, 4))
  test("A scaling matrix applied to a point") {
    /*
​   	  ​Given​ transform ← scaling(2, 3, 4)
​   	    ​And​ p ← point(-4, 6, 8)
​   	   ​Then​ transform * p = point(-8, 18, 32)
    */
    val p = Point(-4, 6, 8)
    val r = Point(-8, 18, 32)
    assert( s * p == colMat(r) )
  }

  val v = Vector(-4, 6, 8)
  test("A scaling matrix applied to a vector") {
    /*
      ​Given​ transform ← scaling(2, 3, 4)
​   	    ​And​ v ← vector(-4, 6, 8)
​   	   ​Then​ transform * v = vector(-8, 18, 32)
    */
    val r = Vector(-8, 18, 32)
    assert( s * v == colMat(r) )
  }

  test("Multiplying by the inverse of a scaling matrix") {
   /*
    ​Given​ transform ← scaling(2, 3, 4)
​   	    ​And​ inv ← inverse(transform)
​   	    ​And​ v ← vector(-4, 6, 8)
​   	   ​Then​ inv * v = vector(-2, 2, 2)
   */
    val sInv = s.inverse
    val r = Vector(-2, 2, 2)
    assert( sInv * v == Some(colMat(r)) )
  }

  val ref = Scale(Vector(-1, 1, 1))
  test("Reflection is scaling by a negative value") {
   /*
    Given​ transform ← scaling(-1, 1, 1)
​   	    ​And​ p ← point(2, 3, 4)
​   	   ​Then​ transform * p = point(-2, 3, 4)
   */
    val p = Point(2, 3, 4)
    val r = Point(-2, 3, 4)
    assert( ref * p == colMat(r) )
  }

  val half_quarter_x = Rotate(Base.X, Math.PI / 4)
  val quarter_x = Rotate(Base.X, Math.PI / 2)
  test("Rotating a point around the x axis") {
    /*
    Given p ← point(0, 1, 0)
      And half_quarter ← rotation_x(π / 4)
      And full_quarter ← rotation_x(π / 2)
    Then half_quarter * p = point(0, √2/2, √2/2)
      And full_quarter * p = point(0, 0, 1)
    */
    val p = Point(0, 1, 0)
    val r1 = Point(0, Math.sqrt(2)/2, Math.sqrt(2)/2)
    val r2 = Point(0, 0, 1)

    assert(half_quarter_x * p == colMat(r1))
    assert(quarter_x * p == colMat(r2))
  }

  test("The inverse of an x-rotation rotates in the opposite direction") {
    import scala.language.strictEquality
    /*
    Given p ← point(0, 1, 0)
      And half_quarter ← rotation_x(π / 4)
      And inv ← inverse(half_quarter)
    Then inv * p = point(0, √2/2, -√2/2)
    */
    val p = Point(0, 1, 0)
    val r1 = Point(0, Math.sqrt(2)/2, -Math.sqrt(2)/2)
    assert((half_quarter_x.inverse * p).get == colMat(r1))
  }

  test("Rotating a point around the y axis") {
    /*
    Given p ← point(0, 0, 1)
      And half_quarter ← rotation_y(π / 4)
      And full_quarter ← rotation_y(π / 2)
    Then half_quarter * p = point(√2/2, 0, √2/2)
      And full_quarter * p = point(1, 0, 0)
    */
    val p = Point(0, 0, 1)
    val half_quarter_y = Rotate(Base.Y, Math.PI / 4)
    val quarter_y = Rotate(Base.Y, Math.PI / 2)
    val r1 = Point(Math.sqrt(2)/2, 0, Math.sqrt(2)/2)
    val r2 = Point(1, 0, 0)
    assert(half_quarter_y * p == colMat(r1))
    assert(quarter_y * p == colMat(r2))
  }

  test("Rotating a point around the z axis") {
    /*
    Given p ← point(0, 1, 0)
      And half_quarter ← rotation_z(π / 4)
      And full_quarter ← rotation_z(π / 2)
    Then half_quarter * p = point(-√2/2, √2/2, 0)
      And full_quarter * p = point(-1, 0, 0)
    */
    val p = Point(0, 1, 0)
    val half_quarter_z = Rotate(Base.Z, Math.PI / 4)
    val quarter_z = Rotate(Base.Z, Math.PI / 2)

    val r1 = Point(-Math.sqrt(2)/2, Math.sqrt(2)/2, 0)
    val r2 = Point(-1, 0, 0)
    assert(half_quarter_z * p == colMat(r1))
    assert(quarter_z * p == colMat(r2))
  }
