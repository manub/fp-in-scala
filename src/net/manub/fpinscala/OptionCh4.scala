package net.manub.fpinscala

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap (a => if (f(a)) Some(a) else None)
}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { avg => mean(xs map { x => math.pow(x-avg, 2) }) }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }

  // is this the right way? is it possible to do this without pattern matching?
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }

  def sequence[A](a: scala.List[Option[A]]): Option[scala.List[A]] =
    Try(a map { elem => elem getOrElse(throw new Exception("!!!")) })


}
