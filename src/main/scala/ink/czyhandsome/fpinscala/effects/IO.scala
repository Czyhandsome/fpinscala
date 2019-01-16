package ink.czyhandsome.fpinscala.effects

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-16
  */
trait IO {
  self =>
  def run(): Unit

  def ++(io: IO): IO = () => { self.run(); io.run() }
}

object IO {
  def empty: IO = () => {}
}

case class PrintLine(msg: String) extends IO {
  override def run(): Unit = println(msg)
}
