package ink.czyhandsome.fpinscala.applicative

/**
  * $DESC
  *
  * @author 曹子钰, 2019-01-12
  */
object Company {
  val F: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      (fa, fb) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        case _ => None
      }
  }

  def main(args: Array[String]): Unit = {
    search()
    search2()
  }

  private def search(): Unit = {
    val name = "Alice"
    val depts = Map((name, "Tom"), ("2", "Lisp"))
    val salaries = Map((name, 12.0), ("2", 14.0))
    val o = F.map2(depts.get(name), salaries.get(name)) {
      (d, s) => {
        s"$name in $d makes $s per year."
      }
    }
    println(o)
  }

  private def search2(): Unit = {
    val name = "Alice"
    val id = 1
    val idsByName = Map((name, id), ("2", 2))
    val depts = Map((id, "Tom"), (2, "Lisp"))
    val salaries = Map((id, 12.0), (2, 14.0))
    val o = idsByName.get(name).flatMap { id =>
      F.map2(depts.get(id), salaries.get(id)) {
        (d, s) => {
          s"$name: Id{$id} in $d makes $s per year."
        }
      }
    }
    println(o)
  }
}
