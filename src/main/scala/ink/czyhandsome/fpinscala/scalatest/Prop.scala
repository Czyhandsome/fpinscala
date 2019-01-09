package ink.czyhandsome.fpinscala.scalatest

import ink.czyhandsome.fpinscala.scalatest.Prop.{FailedCase, SuccessCount, TestCases}
import ink.czyhandsome.fpinscala.states.RNG

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-06
  */
case class Prop(run: (TestCases, RNG) => Result) {
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
