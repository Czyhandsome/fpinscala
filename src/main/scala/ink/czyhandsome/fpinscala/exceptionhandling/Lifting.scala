package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
object Lifting {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Option.Try(age.toInt)
    val optNum = Option.Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optNum) { insuranceRateQuote }
  }
}
