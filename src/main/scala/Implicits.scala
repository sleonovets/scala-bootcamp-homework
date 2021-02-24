package com.bootcamp.homework

object Implicits {

  object TypeclassTask {

    trait HashCode[T] {
      def hash(t: T): Int
    }

    object HashCode {
      def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
    }

    implicit class HashCodeGetter[T](x: T) {
      def hash(implicit t: HashCode[T]): Int = {
        t.hash(x)
      }
    }

    implicit val StringInstance: HashCode[String] = str => str.length

    println("abc".hash)
  }

  object Task1 {

    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount compare y.amount
  }

  object Task2 {

    trait Show[T] { // fancy toString
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    object Show {
      def apply[T](implicit instance: Show[T]): Show[T] = instance
    }

    implicit object ShowUser extends Show[User] {
      def show(user: User): String = s"Hi ${user.name}"
    }

    implicit class ShowEntity[T](x: T) {
      def show(implicit inst: Show[T]): String = {
        inst.show(x)
      }
    }

    User("1", "Oleg").show
  }

  object Task3 {
    type Error = String

    final case class User(id: String, name: String)

    trait Parse[T] { // invent any format you want or it can be csv string
      def parse(entity: String): Either[Error, T]
    }

    object Parse {
      def apply[T](implicit instance: Parse[T]): Parse[T] = instance
    }

    implicit val parseUser: Parse[User] = (input: String) => {
      input.split(',') match {
        case parsed if parsed.length == 2 => Right(User(parsed(0), parsed(1)))
        case _                            => Left("Type Error")
      }
    }

    implicit class ParseEntity(x: String) {
      def parse[T](implicit inst: Parse[T]): Either[Error, T] = {
        inst.parse(x)
      }
    }

    "lalala".parse[User]
    "1, Oleg".parse[User]
  }

  object Task4 {
    // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
    // define the typeclass (think of a method signature)
    // remember `a method b` is `a.method(b)`
  }

  object AdvancedHomework {
    // TODO: create a typeclass for flatMap method
  }

}
