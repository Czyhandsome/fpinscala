package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monoid.Monoid

import scala.language.higherKinds

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A

  def toList[A](as: F[A]): List[A]
}
