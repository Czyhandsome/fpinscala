package ink.czyhandsome.fpinscala.laziness

import ink.czyhandsome.fpinscala.laziness.Stream.{cons, unfold}

/**
  * 无限流
  *
  * @author 曹子钰, 2019-01-01
  */
object InfiniteStream {

  def map_ViaUnfold[A, B](s: Stream[A])(f: A => B): Stream[B] =
    unfold(s) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take_ViaUnfold[A](s: Stream[A], n: Int): Stream[A] =
    unfold((s, n)) {
      case (Cons(h, t), m) if m > 0 => Some(h(), (t(), m - 1))
      case _ => None
    }

  def takeWhile_ViaUnfold[A](s: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(s) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith_ViaUnfold[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((s1, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
      case _ => Some((None: Option[A], None: Option[B]), (Empty, Empty))
    }

  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    zipWith_ViaUnfold(s1, s2).takeWhile { _._2.isDefined }.forAll {
      case (oa, ob) => oa == ob
    }

  def tails[A](s: Stream[A]): Stream[Stream[A]] =
    unfold((s, 1)) {
      case (ss@Cons(_, t), 1) => Some((ss, (t(), 1)))
      case (ss@_, 1) => Some(ss, (Empty, 0))
      case _ => None
    }

  def hasSubSequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    tails(s1) exists { startsWith(_, s2) }

  def scanRight[A, B](s: Stream[A], z: B)(f: (A, B) => B): Stream[B] =
  //    s.foldRight(cons(b, Empty)) { case (a, ss@Cons(h, _)) => cons(p(a, h()), ss) }
    s.foldRight((z, Stream(z))) { (a, p0) =>
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }._2

  def main(args: Array[String]): Unit = {
    println(hasSubSequence(Stream(1, 2, 3), Stream(2)))
    println(hasSubSequence(Stream(1, 2, 3, 4), Stream(2, 3, 4, 5)))
    println(scanRight(Stream(1, 2, 3, 4), 0) { _ + _ }.toList)
  }
}
