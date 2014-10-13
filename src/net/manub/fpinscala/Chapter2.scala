package net.manub.fpinscala

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

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
