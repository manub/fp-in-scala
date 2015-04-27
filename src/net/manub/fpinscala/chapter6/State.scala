package net.manub.fpinscala.chapter6

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }

  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a => s.map { b => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State({ s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  })

}