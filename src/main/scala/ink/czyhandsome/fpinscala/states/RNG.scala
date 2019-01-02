package ink.czyhandsome.fpinscala.states

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-01
  */
trait RNG {
  def nextInt: (Int, RNG)

  def nextNonNegativeInt: (Int, RNG) = {
    val (n, r) = nextInt
    val nn = if (n < 0) -(n + 1) else n
    (nn, r)
  }

  def nextDouble: (Double, RNG) = {
    val (n, r) = nextNonNegativeInt
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
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (aa, ra) = a(rng)
    val (bb, rb) = b(ra)
    (f(aa, bb), rb)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //    fs.foldRight(unit(List[A]()) { (f, r) =>
  //      rng => (f(rng)._1 :: r(rng)._1, f(rng)._2)
  //    }
    fs.foldRight(unit(List[A]())) { (f, acc) => map2(f, acc) { _ :: _ } }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(_.nextInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  val nonNegativeEven: Rand[Int] = {
    val r: Rand[Int] = _.nextNonNegativeInt
    map(r) { i => i - i % 2 }
  }

  def map_viaFlatMap[A, B](f: Rand[A])(g: A => B): Rand[B] =
    flatMap(f) { a => unit(g(a)) }

  def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(a) { aa =>
      flatMap(b) { bb =>
        unit(f(aa, bb))
      }
    }

  val double: Rand[Double] = map(_.nextNonNegativeInt) { it => it.toDouble / (Int.MaxValue + 1) }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val r1: Rand[Int] = _.nextNonNegativeInt
    val r2: Rand[Int] = _.nextInt
    val r3: Rand[Int] = map(_.nextInt) {
      case i if i > 0 => -(i - 1)
      case i@_ => i
    }
    val r4 = map(r3) { _ * 2 }
    val r5 = map(r4) { _ * 3 }
    val r = sequence(List(r1, r2, r3, r4, r5))
    println(r(rng))
  }
}
