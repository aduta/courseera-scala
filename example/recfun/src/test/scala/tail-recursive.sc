object tr {

  def factorialRecurse(product: Int, x: Int): Int =
    if(x == 0) product
    else {
      factorialRecurse(product * x, x - 1)
    }

  def factorial(x: Int): Int =
    if(x == 0) 1
    else {
      factorialRecurse(1, x)
    }

  factorial(5)
}