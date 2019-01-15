package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monad.Functor

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
trait Traverse[F[_]] extends Functor[F] {
  type Const[A, B] = A

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  //  = sequence(map(fa) { f })

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga) { ga => ga }

  // ********** 练习12.14 ********** //
  type Id[D] = D
  private implicit val idApp: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idApp)
}

object Traverse {
  def listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldLeft(G.unit(List[B]())) { (acc, a) => G.map2(f(a), acc) { _ :: _ } }
  }

  def listApp: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }

  def main(args: Array[String]): Unit = {
    import IdApp.{Id => ID}
    val G = listApp
    val T = new Traverse[ID] {
      override def traverse[G[_], A, B](fa: ID[A])(f: A => G[B])(implicit g: Applicative[G]): G[ID[B]] =
        g.map(f(fa.value)) { ID(_) }
    }
    val r = T.sequence(ID(List(1, 2, 3)))(G)
    println(r)
    println(T.map(ID(1)) { "Hello " + _ })
  }
}
