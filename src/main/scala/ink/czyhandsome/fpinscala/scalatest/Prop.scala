package ink.czyhandsome.fpinscala.scalatest

import ink.czyhandsome.fpinscala.laziness.Stream
import ink.czyhandsome.fpinscala.scalatest.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}
import ink.czyhandsome.fpinscala.states.RNG

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-06
  */
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  self =>
  def &&(p: Prop): Prop = Prop { (m, n, rng) =>
    (self.run(m, n, rng), p.run(m, n, rng)) match {
      case (Passed, Passed) => Passed
      case (f@Falsified(_, _), Passed) => f
      case (Passed, f@Falsified(_, _)) => f
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(s"$f1\n$f2", s1 + s2)
    }
  }

  def ||(p: Prop): Prop = Prop { (m, n, rng) =>
    (self.run(m, n, rng), p.run(m, n, rng)) match {
      case (_, Passed) => Passed
      case (Passed, _) => Passed
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(s"$f1\n$f2", s1 + s2)
    }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + max - 1) / max
      val props = Stream
        .from(0)
        .take((n min max) + 1)
        .map(i => forAll(g(i))(f))
      val prop: Prop = props.map { p =>
        Prop {
          (m, _, r) => {
            p.run(m, casesPerSize, r)
          }
        }
      }.toList.reduce { _ && _ }
      prop.run(max, n, rng)
  }

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(g)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng) { rng => Some(g.sample.run(rng)) }

  private def buildMsg[A](a: A, e: Exception): String =
    s"""test case: $a
       generated an exception: ${ e.getMessage }
       stack trace:
       ${ e.getStackTrace.mkString("\n") }
     """
      .stripMargin
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
