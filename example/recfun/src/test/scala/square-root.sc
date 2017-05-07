object session {

  def abs(d: Double) =
    if (d >= 0) d else -d

  def sqrRoot(x: Double): Double = {

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def findBetterGuess(guess: Double): Double =
      (guess + x / guess) / 2

    def sqrRootIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrRootIter(findBetterGuess(guess))

    sqrRootIter(1)
  }

  sqrRoot(4)
  sqrRoot(45)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    var accumulator = 1
    if (a > b) accumulator else  f(a) * product(a+1)
  }

}
