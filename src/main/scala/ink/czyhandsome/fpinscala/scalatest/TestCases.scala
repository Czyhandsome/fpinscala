package ink.czyhandsome.fpinscala.scalatest

import ink.czyhandsome.fpinscala.states.{RNG, SimpleRNG, State}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-06
  */
object TestCases {
  def main(args: Array[String]): Unit = {
    val g = Gen(State(RNG.int))
    val l = g.listOfN(Gen.unit(5))
    var r: RNG = SimpleRNG(42)
    List.range(0, 5)
      .foreach { _ =>
        val rr = l.sample.run(r)
        println(rr)
        r = rr._2
      }
  }
}
