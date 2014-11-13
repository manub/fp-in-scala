package net.manub.fpinscala.chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(value) => Left(value)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) {
      Left("Mean of an empty sequence!")
    } else {
      Right(xs.sum / xs.length)
    }

  def safeDiv(x: Int, y: Int): Either[Exception, Double] = try Right(x / y) catch { case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] = try Right(a) catch { case e: Exception => Left(e) }

}