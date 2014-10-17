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
      case Cons(x, xs) => drop(xs, n - 1)
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

  def length[A](as: List[A]) = foldRight(as, 0)((_, i) => i + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](as: List[A]) = foldLeft(as, List[A]())((h, acc) => Cons(acc, h))


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, acc) => Cons(a, acc))

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def fromDoubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, fromDoubleToString(xs))
  }

  def mapUsingFoldRight[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, acc) => append(List(f(a)), acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
  }

  def foldLeftUsingRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaLeft(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap2(l)(a => if (f(a)) List(a) else Nil)

  def addTwoLists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addTwoLists(xs, ys))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def contains(container: List[A], containee: List[A]): Boolean = {
      (container, containee) match {
        case (_, Nil) => true
        case (Cons(x, xs), Cons(y, ys)) if x == y => contains(xs, ys)
        case _ => false
      }
    }

    sup match {
      case Nil => false
      case Cons(x, xs) => if (contains(sup, sub)) true else hasSubsequence(xs, sub)
    }
  }


}
