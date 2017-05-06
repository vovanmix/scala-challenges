package reductions

import org.scalameter._
import common.{parallel, _}

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  // - Sequential -
  // for each height entry in the input array (except for input(0) - the location of the observer which is always zero),
  // writes the maximum angle until that point into the output array (output(0) should be 0):
  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    def rec(distance: Int, max: Float, input: Array[Float]): Array[Float] =
      if (input.length == 0) output
      else {
        val angle: Float = input.head / distance
        if (angle > max) {
          output(distance) = angle
          rec(distance + 1, angle, input.tail)
        }
        else {
          output(distance) = max
          rec(distance + 1, max, input.tail)
        }
      }

    rec(1, 0, input.tail)
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
    *
    * Returns the maximum angle in a given part of the array.
    */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    def rec(idx: Int, maximum: Float): Float = {
      def angle: Float = input(idx) / idx
      if (idx == until) maximum
      else rec(idx + 1, max(angle, maximum))
    }

    rec(from, input(from))
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   *
   *  Returns the reduction tree over parts of the input array.
   *  If the length of the given part of the input array is less than or equal to threshold,
   *  then upsweep calls upsweepSequential. Note that the part of the input array that needs
   *  to traversed is represented using indices 'from' (inclusive) and 'until' (or 'end') (exclusive).
   */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree =
    if (end - from <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val mid = from + (end - from) / 2
      val(tL, tR) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Node(tL, tR)
    }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   *
   *  Traverses the parts of the array corresponding to leaves of the tree and writes
   *  the final angles into the output array
   */
  def downsweepSequential(input: Array[Float], output: Array[Float], startingAngle: Float, from: Int, until: Int): Unit = {
    def rec(idx: Int, m: Float): Unit = {
      def angle: Float = input(idx) / idx
      if (idx < until)
        if (angle > m){
          output(idx) = angle
          rec(idx + 1, angle)
        }
        else {
          output(idx) = m
          rec(idx + 1, m)
        }
    }

    rec(from, input(from))
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float, tree: Tree): Unit =  tree match {
    case Leaf(from, until, maxPrevious) => downsweepSequential(input, output, startingAngle, from, until)
    case Node(left, right) =>
      val (_, _) = parallel(
        downsweep(input, output, startingAngle, left),
        downsweep(input, output, max(startingAngle, left.maxPrevious), right))
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float], threshold: Int): Unit = {
    val tree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0, tree)
  }
}
