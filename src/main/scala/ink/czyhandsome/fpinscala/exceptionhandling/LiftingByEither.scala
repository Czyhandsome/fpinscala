package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
object LiftingByEither {
  def lift[E, A, B, C](f: (A, B) => C): (Either[E, A], Either[E, B]) => Either[E, C] =
    (a, b) => for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
}
