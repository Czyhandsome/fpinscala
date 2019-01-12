package ink.czyhandsome.fpinscala.applicative

import scala.collection.immutable.Stream.Empty

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-12
  */
object StreamApp {
  val streamApp: Applicative[Stream] = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map f.tupled
  }

  def main(args: Array[String]): Unit = {
    def s[A](n: Int, a: A): Stream[A] = n match {
      case 0 => Empty
      case _ => Stream.cons(a, s(n - 1, a))
    }

    println(s(3, 3).toList)

    val ss = List.range(1, 6).map(i => s(i, i))
    val r = streamApp.sequence(ss).take(10).toList
    println(r)

    val rr = streamApp.sequence(List.fill(5)(s(5, 5))).take(10).toList
    println(rr)
  }
}
