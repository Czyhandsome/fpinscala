package ink.czyhandsome.fpinscala.states

import ink.czyhandsome.fpinscala.states.State._

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-02
  */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def update: Input => Machine => Machine = i => s => {
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map { ip => modify(update(ip)) })
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    val m = Machine(locked = false, 10, 0)
    val s = simulateMachine(List(Coin, Turn, Turn, Coin))
    println(s.run(m))
  }
}
