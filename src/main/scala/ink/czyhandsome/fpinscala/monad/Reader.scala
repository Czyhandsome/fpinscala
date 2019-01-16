package ink.czyhandsome.fpinscala.monad

import ink.czyhandsome.fpinscala.states.State
import ink.czyhandsome.fpinscala.states.State.{get, set}

import scala.language.reflectiveCalls

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-12
  */
case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] = Reader.readerMonad.map(this) { f }

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader.readerMonad.flatMap(this) { f }
}

object Reader {
  def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def flatMap[A, B](a: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r => f(a.run(r)).run(r) }

    override def unit[A](a: => A): Reader[R, A] = Reader { _ => a }
  }

  def main(args: Array[String]): Unit = {
    val s = State { i: Int => (s"Hello $i", i) }
    val ls: List[State[Int, (String, Int)]] = List.range(0, 6).map { _ =>
      for {
        i <- s
        n <- get
        _ <- set(n * 2)
      } yield (i, n)
    }
    val r = StateMonad.stateMonad[Int].sequence(ls).run(1)
    println(r)
  }
}
