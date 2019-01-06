package ink.czyhandsome.fpinscala.parallel.actor

import java.util.concurrent.Executors

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-05
  */
object RunActor {
  def main(args: Array[String]): Unit = {
    val p = Par.parMap(List.range(1, 1000000)) { math.sqrt(_) }
    val x = Par.run(Executors.newFixedThreadPool(4))(p)

    println(x)
  }
}
