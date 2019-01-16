package ink.czyhandsome.fpinscala.effects

import ink.czyhandsome.fpinscala.effects.Player.{winner, winnerMsg}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-16
  */
object IOPlayer {
  def main(args: Array[String]): Unit = {
    val p1 = Player("p1", 1)
    val p2 = Player("p2", 2)
    val io = PrintLine(winnerMsg(winner(p1, p2)))
    io.run()
  }
}
