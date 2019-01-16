package ink.czyhandsome.fpinscala.monad

import ink.czyhandsome.fpinscala.applicative.Applicative
import ink.czyhandsome.fpinscala.laziness.Stream

import scala.language.higherKinds

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-11
  */
trait Monad[M[_]] extends Applicative[M] {
  // ********** primitives ********** //
  def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]

  def unit[A](a: => A): M[A]

  // ********** methods ********** //
  // ********** Non ********** //
  override def map[A, B](a: M[A])(f: A => B): M[B] = flatMap(a) { a => unit(f(a)) }

  def map2[A, B, C](fa: M[A], fb: M[B])(f: (A, B) => C): M[C] =
    flatMap(fa) { a => map(fb) { b => f(a, b) } }

  // ********** 练习11.3 ********** //
  override def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]())) { (fa, acc) => map2(fa, acc) { _ :: _ } }

  override def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
  // sequence(la.map { f })
    la.foldRight(unit(List[B]())) { (a, lb) => map2(f(a), lb) { _ :: _ } }

  // ********** 练习11.4 ********** //
  override def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  // ********** Non ********** //
  override def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb) { (_, _) }

  // ********** 练习11.6 ********** //
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val ff: M[List[Option[A]]] = traverse(ms) {
      a => map2(unit(a), f(a)) { (aa, b) => if (b) Some(aa) else None }
    }
    map(ff) { _.filter(_.isDefined).map { case Some(a) => a } }
  }

  def filterM2[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h)) { b =>
        if (!b) filterM2(t) { f }
        else map(filterM2(t) { f }) { h :: _ }
      }
    }

  // ********** 练习11.7 ********** //
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a)) { g }

  // ********** 练习11.8 ********** //
  private def flatMap_viaComposeUnit[A, B](m: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => m, f)(())
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
