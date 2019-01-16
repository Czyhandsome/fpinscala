package ink.czyhandsome.fpinscala.effects

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-16
  */
case class Player(name: String, score: Int)

object Player {
  def main(args: Array[String]): Unit = {
    val p1 = Player("p1", 1)
    val p2 = Player("p2", 2)
    println(s"$p1 and $p2 plays!")
    contest(p1, p2)
  }

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p match {
    case Some(Player(n, _)) => s"$n is the winner!"
    case _ => "It's a draw!"
  }

  def contest(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))
}
