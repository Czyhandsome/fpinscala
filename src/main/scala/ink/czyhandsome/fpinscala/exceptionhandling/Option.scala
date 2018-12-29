package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this flatMap { it => if (f(it)) Some(it) else None }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case _: Exception => None
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //  a flatMap (aa => b map (bb => f(aa, bb)))
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    //    case x :: xs => x.flatMap(xx => sequence(xs).map(xxs => xx :: xxs))
    case x :: xs => for {
      xx <- x
      xxs <- sequence(xs)
    } yield xx :: xxs
  }

  def sequence_via_foldRight[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil)) { (x, y) => map2(x, y) { _ :: _ } }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f)) { _ :: _ }
  }

  def traverse_via_foldRight[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil)) { (x, y) => map2(f(x), y) { _ :: _ } }

  def sequence_via_traverse[A](a: List[Option[A]]): Option[List[A]] =
    sequence(a) { _ }
}
