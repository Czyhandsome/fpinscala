package ink.czyhandsome.fpinscala.scalatest

import ink.czyhandsome.fpinscala.scalatest.Prop.{FailedCase, SuccessCount}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-06
  */
trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
