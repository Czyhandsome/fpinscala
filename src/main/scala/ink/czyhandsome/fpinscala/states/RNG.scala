package ink.czyhandsome.fpinscala.states

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-01
  */
trait RNG {
  def nextInt: (Int, RNG)

  def nextNonPositiveInt: (Int, RNG) = {
    val (n, r) = nextInt
    val nn = if (n < 0) -(n + 1) else n
    (nn, r)
  }

  def nextDouble: (Double, RNG) = {
    val (n, r) = nextNonPositiveInt
    (n.toDouble / (Int.MaxValue + 1), r)
  }

  def intDouble: ((Int, Double), RNG) = {
    val i = nextInt
    val d = i._2.nextDouble
    ((i._1, d._1), d._2)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val d = nextDouble
    val i = d._2.nextInt
    ((d._1, i._1), i._2)
  }

  def double3: ((Double, Double, Double), RNG) = {
    val d1 = nextDouble
    val d2 = nextDouble
    val d3 = nextDouble
    ((d1._1, d2._1, d3._1), d3._2)
  }

  def ints(count: Int): (List[Int], RNG) = {
    def ints(r: RNG, c: Int, l: List[Int]): (List[Int], RNG) =
      if (c <= 0) (l, r)
      else {
        val (nr, ns) = r.nextInt
        ints(ns, c - 1, nr :: l)
      }

    ints(this, count, Nil)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, SimpleRNG) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(nextSeed)
    val n = (nextSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object RNG {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    println(rng)
    val (r2, rng2) = rng.nextInt
    println((r2, rng2))
    val c3 = rng2.nextDouble
    println(c3)
    val c4 = c3._2.ints(3)
    println(c4)
  }
}
