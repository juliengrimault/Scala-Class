package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r -1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceInternal(chars: List[Char], acc: Integer): Boolean =
      if (chars.isEmpty) acc == 0
      else
        if (chars.head == '(')
          balanceInternal(chars.tail, acc + 1)
        else if (chars.head == ')')
          if (acc > 0)
            balanceInternal(chars.tail, acc - 1)
          else
            false
        else
          balanceInternal(chars.tail, acc)

    balanceInternal(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def combination(money: Int, coins: List[Int]) : Int =
      if (money == 0) 0
      else if (coins.isEmpty) 0
      else
        if (coins.head == money)
          1 + combination(money, coins.tail)
        else if (coins.head < money)
          combination(money - coins.head, coins) +
          combination(money, coins.tail)
        else
          combination(money, coins.tail)

    println(coins.sorted.reverse)
    combination(money, coins.sorted.reverse)
  }
}
