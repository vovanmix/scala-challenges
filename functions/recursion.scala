package recfun

object Main {
    def main(args: Array[String]) {
      println("Pascal's Triangle")
      for (row <- 0 to 10) {
        for (col <- 0 to row)
          print(pascal(col, row) + " ")
        println()
      }
    }
    
    def pascal(c: Int, r: Int): Int =
      if (r == 0) 1
      else if (c == 0) pascal(c, r - 1)
      else if (c == r) pascal(c - 1, r - 1)
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

    def balance(chars: List[Char]): Boolean = {
      def getBalance(braces: List[Char], string: List[Char]): Boolean =
        if (string.isEmpty) braces.isEmpty
        else getBalance(matchBraces(braces, string.head), string.tail)

      def matchBraces(braces: List[Char], head: Char): List[Char] =
        if (!(List('(', ')') contains head)) braces
        else if (braces.nonEmpty && head == ')' && braces.head == '(') braces.tail
        else List(head) ++ braces

      getBalance(List(), chars)
    }
    
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money > 0 && coins.nonEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  }
