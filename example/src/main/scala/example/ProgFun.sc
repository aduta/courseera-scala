object Session {
  1+2
  def abs(x: Double) = if (x < 0) -x else x
}

def sqrIter(guess: Double, x: Double): Double = {
  if(isGoodEnough(guess, x)) guess
  else sqrIter(improve(guess, x), x)
}

def isGoodEnough(guess: Double, x: Double) = {
  val good = math.abs(x - (guess * guess)) < 0.001
//  println(s"New guess: $good")
  good
}

def improve(guess: Double, x: Double) = {
  val mean = (guess + x/guess)/2
//  println(s"New mean: $mean")
  mean
}

def sqrt(x: Double) = sqrIter(1,x)

sqrt(0.001)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e20)
