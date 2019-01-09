package ink.czyhandsome.fpinscala.scalatest

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-09
  */
case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen { g(_) map { f } }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { n => g(n) flatMap { f(_).g(n) } }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen { n => apply(n) ** s2(n) }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { Gen.listOfN(_, g) }
}
