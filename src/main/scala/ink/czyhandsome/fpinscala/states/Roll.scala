package ink.czyhandsome.fpinscala.states

import ink.czyhandsome.fpinscala.states.RNG._

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-02
  */
object Roll {
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6)) { _ + 1 }
}
