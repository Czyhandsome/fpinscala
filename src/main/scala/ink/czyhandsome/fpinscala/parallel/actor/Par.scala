package ink.czyhandsome.fpinscala.parallel.actor

import java.util
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-05
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  implicit def toPar[A](p: Par[A]): InnerPar[A] = InnerPar(p)

  case class InnerPar[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b) { f }
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get
  }

  def unit[A](a: A): Par[A] = _ => new Future[A] {
    override def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override def apply(cb: A => Unit): Unit = eval(es) { a(es)(cb) }
  }

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def asyncFun[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  private def eval(es: ExecutorService)(r: => Unit): util.concurrent.Future[Unit] =
    es.submit(new Callable[Unit] {def call: Unit = r })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    override def apply(cb: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]](es) {
        case Left(aa) => br match {
          case None => ar = Some(aa)
          case Some(bb) => eval(es)(cb(f(aa, bb)))
        }
        case Right(bb) => ar match {
          case None => br = Some(bb)
          case Some(aa) => eval(es)(cb(f(aa, bb)))
        }
      }
      a(es) { combiner ! Left(_) }
      b(es) { combiner ! Right(_) }
    }
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit()) { (a, _) => f(a) }

  def sequence[A](pl: List[Par[A]]): Par[List[A]] = {
    def seqBalance(pl: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
      if (pl.isEmpty) unit(Vector())
      else if (pl.size == 1) map(pl.head) { Vector(_) }
      else {
        val (l, r) = pl.splitAt(pl.size / 2)
        map2(seqBalance(l), seqBalance(r)) { _ ++ _ }
      }

    seqBalance(pl.toIndexedSeq) map { _.toList }
  }

  def parMap[A, B](a: List[A])(f: A => B): Par[List[B]] =
    sequence(a map asyncFun(f))
}

sealed trait Future[A] {
  private[actor] def apply(cb: A => Unit): Unit
}
