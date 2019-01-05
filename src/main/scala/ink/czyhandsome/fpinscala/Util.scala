package ink.czyhandsome.fpinscala

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-03
  */
object Util {
  def calcTime(run: => Unit): Unit = {
    val start = System.currentTimeMillis
    try {
      run
    } finally {
      println(s"Time spent: ${ System.currentTimeMillis - start }ms")
    }
  }
}
