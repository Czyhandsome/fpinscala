package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monad.{Functor, StateMonad}
import ink.czyhandsome.fpinscala.monoid.Monoid
import ink.czyhandsome.fpinscala.states.State
import ink.czyhandsome.fpinscala.states.State.{get, set}

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  //  = sequence(map(fa) { f })

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga) { ga => ga }

  // ********** 练习12.14 ********** //
  type Id[D] = D
  private implicit val idApp: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idApp)

  // ********** Foldable ********** //
  type Const[M, B] = M

  implicit def monoidApp[M](M: Monoid[M]): Applicative[({
    type f[x] = Traverse.this.Const[M, x]
  })#f] = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def unit[A](a: => A): Const[M, A] = M.zero

    override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] =
      M.op(fa, fb)
  }

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApp(mb))

  // ********** States ********** //
  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(StateMonad.stateMonad[S])

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa) { a: A =>
      for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b
    }.run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]()) { (a, s) => ((), a :: s) }._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0) { (a, s) => ((a, s), s + 1) }._1

  // ********** 练习12.16 ********** //
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse) { (_, as) => (as.head, as.tail) }._1

  // ********** 练习12.17 ********** //
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z) { (a, b) => ((), f(b, a)) }._2

  // ********** 组合 ********** //
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("zip: Incompatible shape!")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  // ********** 融合 ********** //
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
}

object Traverse {
  def listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]())) { (a, acc) => G.map2(f(a), acc) { _ :: _ } }
  }

  def listApp: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }

  def main(args: Array[String]): Unit = {
    myFoldLeft()
  }

  import IdApp.{Id => ID}

  val T: Traverse[ID] = new Traverse[ID] {
    override def traverse[G[_], A, B](fa: ID[A])(f: A => G[B])(implicit g: Applicative[G]): G[ID[B]] =
      g.map(f(fa.value)) { ID(_) }
  }
  implicit val G: Applicative[List] = listApp
  implicit val M: Monoid[Int] = new Monoid[Int] {
    override def op(x: Int, y: Int): Int = x + y

    override def zero: Int = 0
  }

  def myId(): Unit = {
    val r = T.sequence(ID(List(1, 2, 3)))
    println(r)
    println(T.map(ID(1)) { "Hello " + _ })
    println(T.foldMap(ID(2)) { _ * 2 }(M))
  }

  def myZip(): Unit = {
    val x = T.traverseS(ID(3)) { a => State[Int, Int] { s => (a, s + 2) } }
    println(x.run(0))
  }

  def myFoldLeft(): Unit = {
    val l = List(1, 2, 3, 4, 5)
    val lt = listTraverse
    val z = lt.foldLeft(l)("Hello ") { _ + _ }
    println(z)
  }
}
