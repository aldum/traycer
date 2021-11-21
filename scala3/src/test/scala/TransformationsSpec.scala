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

    assert( t * p == Some(colMat(r)) )
  }

  test("Multiplying by inverse tm") {
    /*
      Given​ transform ← translation(5, -3, 2)
​   	    ​And​ inv ← inverse(transform)
​   	    ​And​ p ← point(-3, 4, 5)
​   	   ​Then​ inv * p = point(-8, 7, 3)
    */
    val tInv = t.inverse
    val r2 = Point(-8, 7, 3)

    assert( tInv * p == Some(colMat(r2)) )
  }

  test("Transforming a vector") {
    /*
      ​Given​ transform ← translation(5, -3, 2)
​   	    ​And​ v ← vector(-3, 4, 5)
​   	   ​Then​ transform * v = v
    */
    val v = Vector(5, -3, 2)
    assert( t * v == Some(colMat(v)) )
  }

