package ink.czyhandsome.fpinscala.applicative

import java.util.Date

import ink.czyhandsome.fpinscala.monad.Monad

import scala.language.reflectiveCalls

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-12
  */
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  // ********** 练习12.4 ********** //
  def eitherMonad[D]: Monad[({type f[x] = Either[x, D]})#f] =
    new Monad[({type f[x] = Either[x, D]})#f] {
      override def unit[A](a: => A): Either[A, D] = Left(a)

      override def flatMap[A, B](a: Either[A, D])(f: A => Either[B, D]): Either[B, D] =
        a match {
          case Left(aa) => f(aa)
          case Right(bb) => Right(bb)
        }
    }

  // ********** 练习12.5 ********** //
  def validationApp[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C)
      : Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (e@Failure(_, _), Success(_)) => e
        case (Success(_), e@Failure(_, _)) => e
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
      }
    }
}


case class WebForm(name: String, birthday: Date, phoneNumber: String)

object WebForm {
  def validName(name: String): Validation[String, String] =
    if (name.isEmpty) Failure("Name must not be empty!")
    else Success(name)

  def validBirthday(b: String): Validation[String, Date] = try {
    import java.text._
    Success(new SimpleDateFormat("yyyy-MM-dd").parse(b))
  } catch {
    case _: Throwable => Failure("Birthday must be yyyy-MM-dd!")
  }

  def validPhone(phone: String): Validation[String, String] =
    if (phone.matches("[0-9]{10}")) Success(phone)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String, birth: String, phone: String): Validation[String, WebForm] =
    Validation.validationApp.map3(
      validName(name),
      validBirthday(birth),
      validPhone(phone)
    ) { WebForm(_, _, _) }
}
