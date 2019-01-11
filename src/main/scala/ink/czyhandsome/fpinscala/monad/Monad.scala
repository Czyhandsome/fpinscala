package ink.czyhandsome.fpinscala.monad

import ink.czyhandsome.fpinscala.laziness.Stream

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-11
  */
trait Monad[F[_]] {
  // ********** primitives ********** //
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  def unit[A](a: => A): F[A]

  // ********** methods ********** //
  // ********** Non ********** //
  def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a) { a => unit(f(a)) }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa) { a => map(fb) { b => f(a, b) } }

  // ********** 练习11.3 ********** //
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]())) { (fa, acc) => map2(fa, acc) { _ :: _ } }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
  // sequence(la.map { f })
    la.foldRight(unit(List[B]())) { (a, lb) => map2(f(a), lb) { _ :: _ } }

  // ********** 练习11.4 ********** //
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  // ********** Non ********** //
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb) { (_, _) }

  // ********** 练习11.6 ********** //
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val ff: F[List[Option[A]]] = traverse(ms) {
      a => map2(unit(a), f(a)) { (aa, b) => if (b) Some(aa) else None }
    }
    map(ff) { _.filter(_.isDefined).map { case Some(a) => a } }
  }

  def filterM2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h)) { b =>
        if (!b) filterM2(t) { f }
        else map(filterM2(t) { f }) { h :: _ }
      }
    }
}

object Monad {
  // ********** 练习11.1 ********** //
  def optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] =
      a flatMap f

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  def streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] =
      a flatMap f

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  def listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a flatMap f

    override def unit[A](a: => A): List[A] = List(a)
  }

  def main(args: Array[String]): Unit = {
    val lm = listMonad
    val list = List(1, 2, 3, 4, 5)
    val ll = lm.filterM(list) { it => List(it % 2 == 0) }
    println(ll)
    println(lm.filterM2(list) { it => List(it % 2 == 0) })
  }
}
