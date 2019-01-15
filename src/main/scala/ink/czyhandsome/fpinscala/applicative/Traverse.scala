package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.applicative.IdApp.Id
import ink.czyhandsome.fpinscala.monad.Functor

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
trait Traverse[F[_]] extends Functor[F] {
  type Const[A, B] = A

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa) { f })

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga) { ga => ga }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ???
}

object Traverse {
  def listTraverse: Traverse[List] = new Traverse[List] {
    override def sequence[G[_], A](fga: List[G[A]])(implicit G: Applicative[G]): G[List[A]] =
      fga.foldRight(G.unit(List[A]())) { (g, a) => G.map2(g, a) { _ :: _ } }
  }

  def listApp: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }

  def main(args: Array[String]): Unit = {
    val G = listApp
    val T = new Traverse[Id] {
      override def sequence[G[_], A](fga: Id[G[A]])(implicit G: Applicative[G])
      : G[Id[A]] = G.map2(fga.value, G.unit(())) { (g, _) => Id(g) }
    }
    val r = T.sequence(Id(List(1, 2, 3)))(G)
    println(r)
  }
}
