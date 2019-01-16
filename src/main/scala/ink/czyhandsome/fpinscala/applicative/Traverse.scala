package ink.czyhandsome.fpinscala.applicative

import ink.czyhandsome.fpinscala.monad.Functor
import ink.czyhandsome.fpinscala.monoid.Monoid
import ink.czyhandsome.fpinscala.monoid.Monoid.endoMonoid

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-13
  */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G:Applicative[G]): G[F[B]]

  //  = sequence(map(fa) { f })

  def sequence[G[_], A](fga: F[G[A]])(implicit G:Applicative[G]): G[F[A]] =
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
    val T = new Traverse[ID] {
      override def traverse[G[_], A, B](fa: ID[A])(f: A => G[B])(implicit g: Applicative[G]): G[ID[B]] =
        g.map(f(fa.value)) { ID(_) }
    }
    implicit val G: Applicative[List] = listApp
    implicit val M: Monoid[Int] = new Monoid[Int] {
      override def op(x: Int, y: Int): Int = x + y

      override def zero: Int = 0
    }
    val r = T.sequence(ID(List(1, 2, 3)))
    println(r)
    println(T.map(ID(1)) { "Hello " + _ })
    println(T.foldMap(ID(2)) { _ * 2 }(M))
  }
}
