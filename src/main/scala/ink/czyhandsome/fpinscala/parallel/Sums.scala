package ink.czyhandsome.fpinscala.parallel

import java.util.concurrent.Executors

import ink.czyhandsome.fpinscala.Util
import ink.czyhandsome.fpinscala.parallel.Par.{Par, unit}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-03
  */
object Sums {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sum(l), sum(r)) { _ + _ }
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(max(l), max(r)) { Math.max }
    }

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(5)
    val l = List.range(0, 1000000).toIndexedSeq
    Util.calcTime {
      println(max(l)(es))
    }
    Util.calcTime {
      println(l.max)
    }
  }
}
