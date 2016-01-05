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
    if (c == 0 || c == r)  1
    else if (c < 0 || c > r) 0
    else {
        pascal(c-1, r-1) + pascal(c, r-1)
    }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanced(openingBrackets: Int, remaingChars: List[Char]): Boolean =
      if (remaingChars.isEmpty) return true
      else {
        var opened = openingBrackets
        if (remaingChars.head == '(') opened += 1
        else if (remaingChars.head == ')') opened -= 1

        if (opened < 0) false
        else
          balanced(opened, remaingChars.tail)
      }

    return balanced(0, chars)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0 else if (money == 0) 1
    else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

  }

}
