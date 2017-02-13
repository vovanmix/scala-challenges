package reductions

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def getBalance(balance: Int, string: Array[Char]): Boolean =
      if (balance < 0) false
      else if (string.isEmpty) balance == 0
      else if (string.head == ')') getBalance(balance - 1, string.tail)
      else if (string.head == '(') getBalance(balance + 1, string.tail)
      else getBalance(balance, string.tail)

    getBalance(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, opened: Int, closed: Int): (Int, Int) =
      if (idx == until) (opened, closed)
      else
        chars(idx) match {
          case '(' => traverse(idx + 1, until, opened + 1, closed)
          case ')' =>
            if (opened > 0) traverse(idx + 1, until, opened - 1, closed)
            else traverse(idx + 1, until, opened, closed + 1)
          case _ => traverse(idx + 1, until, opened, closed)
        }

    def reduce(from: Int, until: Int): (Int, Int) =
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((leftOpened, leftClosed), (rightOpened, rightClosed)) = parallel(reduce(from, mid), reduce(mid, until))

        if (leftOpened > rightClosed) (leftOpened + rightOpened - rightClosed, leftClosed)
        else (rightOpened, leftClosed + rightClosed - leftOpened)
      }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
  // TODO ^^

}
