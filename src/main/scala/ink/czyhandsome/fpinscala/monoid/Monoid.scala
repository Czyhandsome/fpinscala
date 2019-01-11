package ink.czyhandsome.fpinscala.monoid

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-09
  */
trait Monoid[A] {
  def op(x: A, y: A): A

  def zero: A
}

object Monoid {
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(x: A, y: A): A = m.op(y, x)

    override def zero: A = m.zero
  }

  // ********** 练习10.1 ********** //
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(x: Int, y: Int): Int = x + y

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(x: Int, y: Int): Int = x * y

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(x: Boolean, y: Boolean): Boolean = x || y

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(x: Boolean, y: Boolean): Boolean = x && y

    override def zero: Boolean = true
  }

  // ********** 练习10.2 ********** //
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(x: Option[A], y: Option[A]): Option[A] = x orElse y

    override def zero: Option[A] = None
  }

  // ********** 练习10.3 ********** //
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(x: A => A, y: A => A): A => A = x compose y

    override def zero: A => A = a => a
  }

  // ********** 练习10.5 ********** //
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (b, a) => m.op(b, f(a)) }

  // ********** 练习10.7 ********** //
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.size == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.size / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }
}
