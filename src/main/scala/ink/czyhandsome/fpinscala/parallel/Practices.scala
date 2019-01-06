package ink.czyhandsome.fpinscala.parallel

import java.util.concurrent.Executors

import ink.czyhandsome.fpinscala.parallel.Par._

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-05
  */
object Practices {
  def main(args: Array[String]): Unit = {
    deadLock()
  }

  def deadLock(): Unit = {
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(4)
    P.proc(S) { _ =>
      println(equal(S)(a, fork(a)))
    }
  }
}
