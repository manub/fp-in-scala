import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    @tailrec
    def inner(a: Int, b: Int, n: Int): Int = {
      if (n > 0) inner(b, a+b, n-1) else a
    }

    inner(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i == as.length - 1) true
      else if (!ordered(as(i), as(i+1))) false
      else loop(i+1)
    }

    loop(0)
  }

}
