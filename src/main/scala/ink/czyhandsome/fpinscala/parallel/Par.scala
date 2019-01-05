package ink.czyhandsome.fpinscala.parallel

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-03
  */
object Par {
  implicit def toPar[A](p: Par[A]): InnerPar[A] = InnerPar(p)

  case class InnerPar[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b) { f }
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => UnitFuture(f(a(es).get, b(es).get))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(())) { (a, _) => f(a) }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get())

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncFun[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

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

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps map { asyncFun(f) })

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as map asyncFun { a => if (f(a)) List(a) else List() }
    sequence(pars) map { _.flatten }
  }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = p1(e).get == p2(e).get

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(5)

    def eq[A](a: Par[A], b: Par[A]): Unit = {
      println(equal(es)(a, b))
    }

    eq(unit(1) map { _ + 1 }, unit(2))
  }
}
