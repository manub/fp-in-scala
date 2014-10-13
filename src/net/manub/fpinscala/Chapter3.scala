package net.manub.fpinscala

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {

    @tailrec
    def inner(ds: List[Double], acc: Double): Double = ds match {
      case Nil => acc
      case Cons(x, xs) => inner(xs, x * acc)
    }

    inner(ds, 1)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, List(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(head, xs)
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = n match {
    case 0 => list
    case _ => list match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](list: List[A], pred: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) if pred(x) => dropWhile(xs, pred)
    case _ => list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]) = foldRight(as, 0)((_, i) => i+1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldLeftUsingRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def inverse[A, B](f: (B, A) => B): (A, B) => B = {
      (a, b) => f(b, a)
    }

    as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, f(z, x))(inverse(f))
    }
  }

  def foldRightUsingLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    def inverse[A, B](f: (A, B) => B): (B, A) => B = {
      (a, b) => f(b, a)
    }

    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, inverse(f)(z, x))(inverse(f))
    }
  }
}
