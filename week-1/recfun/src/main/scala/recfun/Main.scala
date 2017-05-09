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

      // pick the head
      // find modulo if mod 0 - increment count and decrement q
      // loop with mod as money and tail as list

      var count = 0
      def div(num: Int, divisor: Int) = (num / divisor, num % divisor)

      val (q, mod) = div(money, coins.head)
      if (q == 0) {
        count = count + countChange(mod, coins.tail)
      } else {
        if (mod == 0) {

        }
        count = count + 1 + countChange(mod, coins.tail)

      }


      count
    }
  }
