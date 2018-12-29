package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
trait My[+E, +A] {
  def map[B](f: A => B): My[E, B] = this match {
    case Bad(e) => Bad(e)
    case Good(a) => Good(f(a))
  }

  def flatMap[EE >: E, B](f: A => My[EE, B]): My[EE, B] = this match {
    case Bad(e) => Bad(e)
    case Good(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](default: => My[EE, AA]): My[EE, AA] = this match {
    case Bad(_) => default
    case Good(a) => Good(a)
  }
}

case class Bad[E](value: Seq[E]) extends My[E, Nothing]

case class Good[A](value: A) extends My[Nothing, A]

object My {
  def map2[E, A, B, C](a: My[E, A], b: My[E, B])(f: (A, B) => C): My[E, C] = (a, b) match {
    case (Bad(e1), Bad(e2)) => Bad(e1 ++ e2)
    case (Bad(e1), _) => Bad(e1)
    case (_, Bad(e2)) => Bad(e2)
    case (Good(a1), Good(a2)) => Good(f(a1, a2))
  }

  def traverse[E, A, B](a: List[A])(f: A => My[E, B]): My[E, List[B]] = a match {
    case Nil => Good(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f)) { _ :: _ }
  }

  def traverse_via_foldRight[E, A, B](a: List[A])(f: A => My[E, B]): My[E, List[B]] =
    a.foldRight[My[E, List[B]]](Good(Nil)) { (a, l) => map2(f(a), l) { _ :: _ } }

  def sequence[E, A](a: List[My[E, A]]): My[E, List[A]] =
    traverse(a) { x => x }
}
