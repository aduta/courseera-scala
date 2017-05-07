def factorial(n: Int): Int = {

  def facto(acc: Int, n: Int): Int =
    if (n == 0) acc else facto(acc * n, n - 1)

  facto(1, n)
}

factorial(4)
factorial(5)
factorial(2)