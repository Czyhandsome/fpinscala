package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monad.Functor

import scala.language.higherKinds

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-12
  */
// ********** 练习12.2 ********** //
trait Applicative2[F[_]] extends Functor[F] {
  // ********** primitives ********** //
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  // ********** Methods ********** //
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
  //    apply(apply(unit(f.curried))(fa))(fb)
  {
    val f1: F[A] => F[B => C] = apply(unit(f.curried))
    val f2: F[B] => F[C] = apply(f1(fa))
    f2(fb)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
}
