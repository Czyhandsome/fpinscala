package ink.czyhandsome.fpinscala.exceptionhandling

/**
  * $DESC
  *
  * @author 曹子钰, 2018/12/29 0029
  */
object Testimony {
  def main(args: Array[String]): Unit = {
    println(Person.mkPerson(null, -1))
    println(BetterPerson.mkPerson(null, -2))
  }
}

case class Person(name: Name,
                  age: Age)

sealed case class Name(value: String)

sealed case class Age(value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty!")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range!")
    else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    Either.map2(mkName(name), mkAge(age)) { Person(_, _) }
}

object BetterPerson {
  def mkName(name: String): My[String, Name] =
    if (name == "" || name == null) Bad(Seq("List is empty!"))
    else Good(Name(name))

  def mkAge(age: Int): My[String, Age] =
    if (age < 0) Bad(Seq("Age is out of range!"))
    else Good(Age(age))

  def mkPerson(name: String, age: Int): My[String, Person] =
    My.map2(mkName(name), mkAge(age)) { Person(_, _) }
}
