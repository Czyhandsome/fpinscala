package ink.czyhandsome.fpinscala.monad

import ink.czyhandsome.fpinscala.monad.State.{getState, setState}

import scala.language.reflectiveCalls

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-11
  */
object StateMonad {
  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    override def flatMap[A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] = a flatMap f

    override def unit[A](a: => A): State[S, A] = State.unit(a)
  }

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(stateMonad[Int].unit(List[(Int, A)]())) { (acc, a) =>
      for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs
    }.run(0)._1.reverse

  def main(args: Array[String]): Unit = {
    val z = zipWithIndex(List(1, 2, 3, 4, 5, 6, 7))
    println(z)
  }
}

case class State[S, A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }

  def map[B](f: A => B): State[S, B] = State {
    s => {
      val (a, s1) = run(s)
      (f(a), s1)
    }
  }
}

object State {
  def unit[S, A](a: => A): State[S, A] = State { (a, _) }

  def getState[S]: State[S, S] = State { s => (s, s) }

  def setState[S](s: S): State[S, Unit] = State { _ => (s, s) }
}
