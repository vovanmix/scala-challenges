package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 4 empty quadrants ") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f)
    assert(quad.centerY == 30f)
    assert(quad.mass ~= 0f)
    assert(quad.massX ~= 20f)
    assert(quad.massY ~= 30f)
    assert(quad.total == 0)
  }

  test("Fork.insert(b) should insert recursively in the appropriate quadrant") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1, b2))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    var quad = Fork(nw, ne, sw, se)

    quad = quad.insert(b3)
    assert(quad.total == 3)
    assert(quad.mass == 892.5f)
    assert(quad.massX == 23.02773f)
  }

  test("Fork.insert(b) should insert recursively in the appropriate quadrant 2") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    var quad = Fork(nw, ne, sw, se)

    quad = quad.insert(b1)
    quad = quad.insert(b2)
    quad = quad.insert(b3)
    assert(quad.total == 3)
    assert(quad.massX == 23.02773f)
  }

  test("'insert' should work correctly on a leaf with center (1,1) and size <= minimumSize") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)
    val leaf = Leaf(17.5f, 27.5f, 0.000001f, Seq(b1, b2))

    val newLeaf = leaf.insert(b3)
    assert(newLeaf.massX == 23.02773f)
  }

  test("Leaf.insert(b) should return a new Fork if size > minimumSize") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)
    val leaf = Leaf(17.5f, 27.5f, 0.1f, Seq(b1, b2))

    val fork = leaf.insert(b3)
    assert(fork.massX == 23.02773f)
  }

  test("Leaf.insert(b) should return a new Fork if size > minimumSize 2") {
    val b1 = new Body(123f, 0.01f, 0f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val leaf = Leaf(17.5f, 27.5f, 0.1f, Seq(b1))

    val fork = leaf.insert(b2)
    val res: Int = fork match {
      case Fork(nw, _, _, _) => nw match {
        case Leaf(_, _, _, _) => 1
        case Empty(_, _, _) => 2
        case _ => 3
      }
      case _ => 4
    }
    assert(res == 1)
  }

//    [Test Description] Leaf.insert(b) should return a new Fork if size > minimumSize
//    [Observed Error] nw of the Fork, Empty(2.5,2.5,5.0), should be a Leaf
//    [Lost Points] 2

//    [Test Description] Fork.insert(b) should insert recursively in the appropriate quadrant
//    [Observed Error] nw of the Fork, Empty(2.5,2.5,5.0), should be a Leaf
//    [Lost Points] 2

//    [Test Description] 'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector
//    [Observed Error] ConcBuffer() had size 0 instead of expected size 1 bucket (0,2) should have size 1
//    [Lost Points] 2

//    [Test Description] 'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96
//    [Observed Error] res was false Body 1 not found in the right sector in the sector matrix
//    [Lost Points] 2

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector") {
    val body = new Body(5, 4, 4, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body

    val body2 = new Body(5, 5, 4, 0.1f, 0.1f)
    val body3 = new Body(5, 44, 88, 0.1f, 0.1f)
    val body4 = new Body(5, 98, 98, 0.1f, 0.1f)
    val boundaries2 = new Boundaries()
    boundaries2.minX = 3
    boundaries2.minY = 3
    boundaries2.maxX = 99
    boundaries2.maxY = 99
    val sm2 = new SectorMatrix(boundaries2, SECTOR_PRECISION)
    sm2 += body2
    sm2 += body3
    sm2 += body4

    val sm3 = sm combine sm2
    assert(sm3.apply(0, 0).size == 2)
  }

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)") {
    val body1 = new Body(5, 12, 34, 0.1f, 0.1f)
    val body2 = new Body(5, 23, 45, 0.1f, 0.1f)
    val body3 = new Body(5, 56, 9, 0.1f, 0.1f)
    val body4 = new Body(5, 8, 79, 0.1f, 0.1f)
    val body5 = new Body(5, 5, 99, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body1
    sm += body2

    val boundaries2 = new Boundaries()
    boundaries2.minX = 3
    boundaries2.minY = 3
    boundaries2.maxX = 99
    boundaries2.maxY = 99
    val sm2 = new SectorMatrix(boundaries2, SECTOR_PRECISION)
    sm2 += body3
    sm2 += body4
    sm2 += body5

    val sm3 = sm combine sm2
    assert(sm3(4, 0).size == 1 && sm3(4, 0).exists(_ == body3))
  }
}


object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}
