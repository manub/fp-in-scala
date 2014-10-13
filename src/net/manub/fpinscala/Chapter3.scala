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
}
