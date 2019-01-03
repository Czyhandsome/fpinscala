package ink.czyhandsome.fpinscala.states

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-02
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = this flatMap { a => State.unit(f(a)) }

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] =
    this flatMap { aa => b map { bb => f(aa, bb) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, ss) = run(s)
      f(a).run(ss)
    }
}

trait RNG {
  type Rand[A] = State[RNG, A]
}

object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil)) { (f, acc) => f.map2(acc) { _ :: _ } }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
