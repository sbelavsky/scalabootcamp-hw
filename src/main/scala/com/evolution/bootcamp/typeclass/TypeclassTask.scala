package com.evolution.bootcamp.typeclass

object TypeclassTask {
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  // I didn't get it why do we need the apply method :( It works without it
  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit h: HashCode[A]): Int = {
      h.hash(x)
    }
  }

  implicit val stringToHash: HashCode[String] = _.hashCode

  "abc".hash

}
