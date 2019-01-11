package ink.czyhandsome.fpinscala.monad

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-11
  */
case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id(f(value))
}

object Id {

  implicit class IdMonad[D](val id: Id[D]) extends Monad[Id] {
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = a flatMap f

    override def unit[A](a: => A): Id[A] = Id(a)

    def map[B](f: D => B): Id[B] = map(id) { f }

    def flatMap[B](f: D => Id[B]): Id[B] = flatMap(id) { f }

    def product[B](b: Id[B]): Id[(D, B)] = product(id, b)
  }

  def main(args: Array[String]): Unit = {
    val r = for {
      s1 <- Id("Hello, ")
      s2 <- Id("Monad!")
    } yield s1 + s2
    println(r)
  }
}
