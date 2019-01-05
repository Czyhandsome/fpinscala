package ink.czyhandsome.fpinscala.parallel

import java.util.concurrent.ExecutorService

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-05
  */
object P {
  def proc(es: ExecutorService)(f: => Unit): Unit = {
    try {
      f
    } finally {
      es.shutdownNow()
    }
  }
}
