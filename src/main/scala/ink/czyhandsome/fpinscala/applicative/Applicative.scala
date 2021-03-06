package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monad.Functor

import scala.language.{higherKinds, reflectiveCalls}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-12
  */
trait Applicative[F[_]] extends Functor[F] {
  // ********** primitives ********** //
  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  // ********** Methods ********** //
  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(())) { (aa, _) => f(aa) }

  def traverse[A, B](fas: List[A])(f: A => F[B]): F[List[B]] =
    fas.foldRight(unit(Nil: List[B])) { (a, acc) => map2(f(a), acc) { _ :: _ } }

  // ********** 练习12.1 ********** //
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas) { a => a }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb) { (_, _) }

  // ********** 练习12.2 ********** //
  def apply[A, B](f: F[A => B])(fa: F[A]): F[B] = map2(fa, f) { (a, b) => b(a) }

  // ********** 练习12.3 ********** //
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // ********** 练习12.8 ********** //
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (Applicative.this.map2(fa._1, fb._1) { f }, G.map2(fa._2, fb._2) { f })
    }

  // ********** 练习12.12 ********** //
  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] = {
    map(traverse(m.toList) { m => product(unit(m._1), m._2) }) {
      m => m.map { it => it._1 -> it._2 }.toMap
    }
  }

  def sequenceMap2[K, V](m: Map[K, F[V]]): F[Map[K, V]] =
    m.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, f)) =>
      map2(acc, f) { (m, v) => m + (k -> v) }
    }
}
