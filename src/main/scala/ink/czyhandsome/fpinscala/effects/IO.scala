package ink.czyhandsome.fpinscala.effects

import ink.czyhandsome.fpinscala.monad.Monad

import scala.io.StdIn

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-16
  */
trait IO[A] {
  self =>
  def run(): A

  def map[B](f: A => B): IO[B] = () => f(self.run())

  def flatMap[B](f: A => IO[B]): IO[B] = () => f(self.run()).run()
}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = () => a

  override def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  // ********** Helpers ********** //
  def ReadLine: IO[String] = () => StdIn.readLine()

  def PrintLine(msg: String): IO[Unit] = () => println(msg)
}
