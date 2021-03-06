package kmeans

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point,GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  def checkUpdate(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point], expected: GenSeq[Point]) {
    assert(update(classified, oldMeans).toString == expected.toString)
  }

  test("update works fine") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val m1 = new Point(-11, -11, 0)

    val classified = GenMap(m1 -> GenSeq(p1, p2, p3, p4))
    val oldMeans = GenSeq(m1)
    val expected = GenSeq(new Point(0.0, 0.0, 0.0))
    checkUpdate(classified, oldMeans, expected)
  }

  test("update works fine 2") {
    val p1 = new Point(1, 8, -1)
    val p2 = new Point(2, 7, -2)
    val p3 = new Point(3, 6, -2)
    val p4 = new Point(4, 5, -2)
    val m1 = new Point(0, 0, 0)

    val classified = GenMap(m1 -> GenSeq(p1, p2, p3, p4))
    val oldMeans = GenSeq(m1)
    val expected = GenSeq(new Point(2.5, 6.5, -1.75))
    checkUpdate(classified, oldMeans, expected)
  }

  def checkConverged(eta: Double, oldMeans: GenSeq[Point], newMeans: GenSeq[Point], expected: Boolean) {
    assert(converged(eta)(oldMeans, newMeans) == expected)
  }

  test("correctly detects converged if all not converged") {
    checkConverged(
      2,
      GenSeq(
        new Point(1, 8, -1),
        new Point(2, 7, -1),
        new Point(3, 6, -1)
      ),
      GenSeq(
        new Point(3, 8, -1),
        new Point(4, 8, -1),
        new Point(5, 8, -1)
      ),
      false
    )
  }

  test("correctly detects converged if only one not converged") {
    checkConverged(
      2,
      GenSeq(
        new Point(3, 8, -1),
        new Point(1, 2, -1),
        new Point(3, 6, -1)
      ),
      GenSeq(
        new Point(3, 8, -1),
        new Point(2, 7, -1),
        new Point(3, 6, -1)
      ),
      false
    )
  }

  test("correctly detects all converged") {
    checkConverged(
      2,
      GenSeq(
        new Point(1, 8, -1),
        new Point(2, 7, -1),
        new Point(3, 6, -1)
      ),
      GenSeq(
        new Point(2, 8, -1),
        new Point(2, 7, 0),
        new Point(3, 5, -1)
      ),
      true
    )
  }
}
