package ink.czyhandsome.fpinscala.effects

import ink.czyhandsome.fpinscala.effects.IO.{PrintLine, ReadLine}

import scala.io.StdIn

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-16
  */
object IOTemperature {
  def f2c(f: Double): Double = (f - 32) * 5.0 / 9.0

  def main(args: Array[String]): Unit = {
    converter2()
  }

  def converter(): Unit = {
    println("Enter a F: ")
    val f = StdIn.readLine().toDouble
    println(f2c(f))
  }

  def converter2(): Unit = {
    val c: IO[Unit] = for {
      _ <- PrintLine("Please input f: ")
      f <- ReadLine map { _.toDouble }
      _ <- PrintLine(f2c(f).toString)
    } yield ()
    c.run()
  }
}
