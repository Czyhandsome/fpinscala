package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
object Algorithms {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => Some(xs.map(x => math.pow(x - m, 2)).sum))
}
