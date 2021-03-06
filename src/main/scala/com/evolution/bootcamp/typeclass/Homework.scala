package com.evolution.bootcamp.typeclass

// make as many exercises as you can

object Task1 {
  final case class Money(amount: BigDecimal)
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val showUser: Show[User] = u => u.toString()

  implicit class ShowSyntax[A](x: A) {
    def show(implicit s: Show[A]): String = {
      s.show(x)
    }
  }

  User("1", "Oleg").show
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit val parse: Parse[User] = s =>
    s.replaceAll("\\s", "") match {
      case s"""{"id":"$id","name":"$name"}""" => Right(User(id, name))
      case _                                  => Left("can't parse the user")
    }

  implicit class ParseOps[T](s: String) {
    def parse(implicit parser: Parse[T]): Either[Error, T] = {
      parser.parse(s)
    }
  }

  "lalala".parse
  """{"id": "some", "name": "lel"}""".parse

}

object Task4 {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  trait Equals[T] {
    def ===(a: T, b: T): Boolean
  }

  implicit def equals[T]: Equals[T] = (a, b) =>
    (a, b) match {
      case (x, y) => x == y
      case _      => false
    }

  implicit class EqualsOps[T](x: T) {
    def ===(that: T)(implicit eq: Equals[T]) = {
      eq.===(x, that)
    }
  }

  "a" === "a"
}

object AdvancedHomework {
  trait FlatMap[T] {
    def flatMap(list: List[T], f: T => List[T]): List[T]
  }

  implicit def flatMap[T]: FlatMap[T] = (list, f) => {
    def recur(list: List[T], f: T => List[T]): List[T] = {
      list match {
        case Nil          => Nil
        case head :: next => f(head) ::: recur(next, f)
      }
    }
    recur(list, f)
  }

  implicit class FlatMapOps[T](x: List[T]) {
    def coolFlatMap(f: T => List[T])(implicit fm: FlatMap[T]): List[T] = {
      fm.flatMap(x, f)
    }
  }

}
