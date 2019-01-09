package ink.czyhandsome.fpinscala.scalatest

import ink.czyhandsome.fpinscala.states.{RNG, State}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-06
  */
case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample map { f })

  def flatMap[B](f: A => Gen[B]) =
    Gen(sample flatMap { it => f(it).sample })

  def listOfN(i: Gen[Int]): Gen[List[A]] =
    i.flatMap(ii => Gen.listOfN(ii, this))

  def unsized: SGen[A] = SGen { _ => this }

  def **[B](g2: Gen[B]): Gen[(A, B)] = for {
    a1 <- this
    a2 <- g2
  } yield (a1, a2)
}

object Gen {
  def choose(s: Int, e: Int): Gen[Int] = Gen(State { rng =>
    val (i, r) = rng.nextDouble
    ((i * (s - e)).toInt + s, r)
  })

  def unit[A](a: => A): Gen[A] = Gen(State { (a, _) })

  def boolean: Gen[Boolean] = Gen {
    State { rng =>
      rng.nextDouble match {
        case (a, s) => (a > 0.5, s)
      }
    }
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n) { g.sample }))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap { if (_) g1 else g2 }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = (g1, g2) match {
    case ((gg1, w1), (gg2, w2)) => Gen(State(RNG.double)) flatMap { it =>
      if (it > w1 / (w1 + w2)) gg1 else gg2
    }
  }
}
