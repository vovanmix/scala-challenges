package example

object Main extends App {
  println(Lists.max(List(1, 3, 2)))

  Solution.main(Array())
}



object Solution {
  def sum(xs: Array[Int]): Int =
    if (xs.isEmpty) 0
    else xs.head + sum(xs.tail)

  def main(args: Array[String]) {
    val _ = scala.io.StdIn.readInt()
    val input = scala.io.StdIn.readLine()
    val arr: Array[Int] = input.split(" ").map(x => x.toInt)
    println(arr.reduce((x, a) => x + a))
  }
}