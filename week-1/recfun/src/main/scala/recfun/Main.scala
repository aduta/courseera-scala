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

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {

//  print(s"Called pascal($r,$c)");
  if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
}
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def next(chars: List[Char], open: Int): (List[Char], Int) = {
        if (chars.isEmpty) (chars, open)
        else if (chars.head == '(') (chars.tail, open + 1)
        else if (chars.head == ')') (chars.tail, open - 1)
        else next(chars.tail, open)
      }

      def matches(chars: List[Char], open: Int): Boolean = {

        if (chars.isEmpty) open == 0
        else if (open < 0) false
        else {
          val t = next(chars, open)
          matches(t._1, t._2)
        }
      }

      matches(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countForHead(money: Int, coins: List[Int]) = {

        if (money == 0) 1
        else {
          var acc = 0
          for (i <- 1 to (money / coins.head)) {
            acc = acc + countChange(money - i * coins.head, coins.tail)
          }

          acc
        }
      }

      if (money == 0) {
        println ("money is zero")
        1
      }
      else if (coins.isEmpty || money < 0) {
        println(s"coins list is ${coins} and money is $money")
        0
      }
      else {
        println (s"Recursing over $money and $coins")
        val q = money / coins.head
        val mod = money % coins.head

        countForHead(money, coins) + countChange(money, coins.tail)
      }


    }
  }
