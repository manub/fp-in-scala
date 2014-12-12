package net.manub.fpinscala.chapter5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Stream[A] = if (n == 0) Empty else this match {
    case Empty => Stream.empty
    case Cons(h, t) => Stream.cons(h(), t().take(n-1))
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Stream.empty
    case Cons(h, t) => if (p(h())) Stream.cons(h(), t().takeWhile(p)) else Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsFoldRight(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def takeWhileFolded(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}