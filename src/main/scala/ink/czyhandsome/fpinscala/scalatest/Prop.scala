package ink.czyhandsome.fpinscala.scalatest

import ink.czyhandsome.fpinscala.scalatest.Prop.{FailedCase, SuccessCount, TestCases}
import ink.czyhandsome.fpinscala.states.RNG

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-06
  */
case class Prop(run: (TestCases, RNG) => Result) {
  self =>
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    (self.run(n, rng), p.run(n, rng)) match {
      case (Passed, Passed) => Passed
      case (f@Falsified(_, _), Passed) => f
      case (Passed, f@Falsified(_, _)) => f
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(s"$f1\n$f2", s1 + s2)
    }
  }

  def ||(p: Prop): Prop = Prop { (n, rng) =>
    (self.run(n, rng), p.run(n, rng)) match {
      case (_, Passed) => Passed
      case (Passed, _) => Passed
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(s"$f1\n$f2", s1 + s2)
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Result = Either[(FailedCase, SuccessCount), SuccessCount]
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
