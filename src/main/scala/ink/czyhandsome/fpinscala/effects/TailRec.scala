package ink.czyhandsome.fpinscala.effects

import scala.annotation.tailrec

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-19
  */
sealed trait TailRec[A] {
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

  @tailrec
  final def run(): A = this match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => f(a).run()
      case Suspend(r) => f(r()).run()
      case FlatMap(y, g) => (y flatMap (a => g(a) flatMap f)).run()
    }
  }
}

case class Return[A](a: A) extends TailRec[A]

case class Suspend[A](resume: () => A) extends TailRec[A]

case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {

  override def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f

  override def unit[A](a: => A): TailRec[A] = Return(a)

  def printLine(s: String): TailRec[Unit] = Suspend(() => Return(println(s)))

  def main(args: Array[String]): Unit = {
    forever(printLine("Hello")).run()
  }
}
