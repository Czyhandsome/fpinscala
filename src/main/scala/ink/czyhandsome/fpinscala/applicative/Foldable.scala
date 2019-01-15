package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monoid.Monoid
import ink.czyhandsome.fpinscala.monoid.Monoid.endoMonoid

import scala.language.higherKinds

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
trait Foldable[F[_]] {
  // ********** primitives ********** //
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  // ********** Methods ********** //
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as)(a => (b: B) => f(b, a))(Monoid.dual(endoMonoid[B]))(z)
  }

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldMap(as) { m.op(m.zero, _) }(m)

  def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]()) { _ :: _ }
}
