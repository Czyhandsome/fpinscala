package ink.czyhandsome.fpinscala.laziness

import ink.czyhandsome.fpinscala.laziness.Stream.cons

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * $DESC
  *
  * @author 曹子钰, 2018-12-30
  */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    val buf = ListBuffer[A]()

    @tailrec
    def toList(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        toList(t())
      case _ => buf.toList
    }

    toList(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = {
    def dropped(s: Stream[A], m: Int): Stream[A] =
      if (m <= 0) s
      else s match {
        case Empty => Empty
        case Cons(_, t) => dropped(t(), m - 1)
      }

    dropped(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean =
    this.foldRight(false) { (a, b) => p(a) || b }

  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
    case _ => b
  }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true) { (a, b) => p(a) && b }

  def takeWhile_viaFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A]) {
      (h, t) =>
        if (p(h)) cons(h, t)
        else Empty
    }

  def headOption_viaFoldRight(): Option[A] =
    this.foldRight(None: Option[A]) { (a, _) => Some(a) }

  def map[B](p: A => B): Stream[B] =
    this.foldRight(Empty: Stream[B]) { (a, s) => cons(p(a), s) }

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A]) { (a, s) =>
      if (p(a)) cons(a, s)
      else s
    }

  def append[AA >: A](a: => Stream[AA]): Stream[AA] =
    this.foldRight(a) { (a, s) => cons(a, s) }

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    this.foldRight(Empty: Stream[B]) { (a, s) => p(a) append s }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = t1
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(s.flatMap(it => cons(it, cons(it * 2, Empty))).toList)
    println(s.append(Stream(15, 16)).toList)
  }
}
