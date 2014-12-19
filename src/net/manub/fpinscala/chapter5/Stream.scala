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

  def existsWithFoldRight(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  def headOptionWithFoldRight: Option[A] = this.foldRight(None: Option[A])((a, b) => Some(a))

  def mapWithFoldRight[B](f: A => B): Stream[B] =
    this.foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filterWithFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def appendWithFoldRight[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t)=>Stream.cons(h,t))

  def flatMapWithFoldRight[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B])((a, b) => Stream.append(f(a), b))
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

  def append[A](a1: Stream[A], a2: Stream[A]): Stream[A] = a1 match {
    case Empty => a2
    case Cons(h, t) => Stream.cons(h(), append(t(), a2))
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def fib(n1: Int, n2: Int): Stream[Int] = Stream.cons(n1, fib(n2, n1 + n2))

    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  def fibsWithUnfold: Stream[Int] = {
    unfold((0, 1)){case (n1, n2) => Some(n1, (n2, n1 + n2))}
  }

  def fromWithUnfold(n: Int): Stream[Int] = {
    unfold(n){case m => Some(m, m+1)}
  }

}