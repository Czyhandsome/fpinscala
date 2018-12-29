package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => default
    case Right(a) => Right(a)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def map2[EE >: E, E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
}
