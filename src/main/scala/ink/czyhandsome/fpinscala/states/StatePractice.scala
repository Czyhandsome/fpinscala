package ink.czyhandsome.fpinscala.states

import ink.czyhandsome.fpinscala.states.State.{get, set, unit}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-16
  */
object StatePractice {
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    val ll = l.foldLeft(unit[Int, List[_]](List[(Int, Int)]())) { (acc, a) =>
      for {
        as <- acc
        s <- get[Int]
        _ <- set(s + 1)
      } yield (s, a) :: as
    }.run(0)
    println(ll)
  }
}
