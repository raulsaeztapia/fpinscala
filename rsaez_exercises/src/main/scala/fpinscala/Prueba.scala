import scala.annotation.tailrec

object Prueba {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      println(cur)
      if (n == 1) prev else loop(n - 1, cur, prev + cur)
    }
    loop(n, 0, 1)
  }

  def fib3(n: Long): Long = {
    @tailrec
    def fib_tail(n: Long, a: Long, b: Long): Long = n match {
      case 0 => a
      case _ => println(b); fib_tail(n - 1, b, a + b)
    }
    fib_tail(n, 0, 1)
  }

  def fib4(n: Double): Double = {
    @tailrec
    def fib_tail(n: Double, a: Double, b: Double): Double = n match {
      case 0 => a
      case _ => println(b); fib_tail(n - 1, b, (a + b))
    }
    fib_tail(n, 0, 1)
  }

  def factorial(n: Int): Int = {
    if (n == 0)
      return 1
    else
      return n * factorial(n - 1)
  }

  def factorial2(n: Double): Double = {
    def factorialAccumulator(acc: Double, n: Double): Double = {
      if (n == 0) acc
      else {
        println(acc)
        factorialAccumulator(n * acc, n - 1)
      }
    }
    factorialAccumulator(1, n)
  }

}
