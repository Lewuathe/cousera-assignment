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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceStack(parentheses: List[Char], restOfParenthese: Int): Boolean = {
      if (restOfParenthese < 0) false
      else if (parentheses.isEmpty && restOfParenthese == 0) true
      else if (parentheses.isEmpty && restOfParenthese != 0) false
      else if (parentheses.head == '(') balanceStack(parentheses.tail, restOfParenthese + 1)
      else if (parentheses.head == ')') balanceStack(parentheses.tail, restOfParenthese - 1)
      else balanceStack(parentheses.tail, restOfParenthese)
    }

    balanceStack(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0) 0
      else if(coins.isEmpty && money >=1 ) 0
      else count(money, coins.tail) + count(money - coins.head, coins)
    }

    count(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
