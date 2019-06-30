object ch1{
  def main(args: Array[String]): Unit = {
    println(fib(5))

    val xs: Array[Int] = 1.to(10).toArray.reverse
    println(isSorted(xs, (x: Int, y: Int) => x <= y))


  }

  def fib(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int = {
      if (n <= 0) a
      else go(b, a + b, n - 1)
    }
    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C):(A,B) => C = {
    (a,b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
