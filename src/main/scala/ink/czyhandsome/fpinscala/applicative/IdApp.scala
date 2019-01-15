package ink.czyhandsome.fpinscala.applicative

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
object IdApp {

  case class Id[A](value: A)

  def idApp: Applicative[Id] = {
    new Applicative[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
        (fa, fb) match { case (Id(a), Id(b)) => Id(f(a, b)) }
    }
  }

  def main(args: Array[String]): Unit = {
    val A = idApp

    val m = Map((1, Id(2)), (3, Id(3)))
    println(A.sequenceMap(m))
    println(A.sequenceMap2(m))
  }
}
