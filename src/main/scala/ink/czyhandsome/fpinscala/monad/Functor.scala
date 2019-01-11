package ink.czyhandsome.fpinscala.monad

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-11
  */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab) { _._1 }, map(fab) { _._2 })

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(a) => map(a) { Left(_) }
    case Right(b) => map(b) { Right(_) }
  }
}

object Functor {
  def listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }
}
