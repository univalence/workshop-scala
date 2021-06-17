package io.univalence.workshop

object Todo {

  final case class Box[+A](a: A) {
    def map[B](f: A => B): Box[B] = ???
    def flatMap[B](f: A => Box[B]): Box[B] = ???
  }

  sealed trait Option[+A] {
    def map[B](f: A => B): Box[B] = ???
    def flatMap[B](f: A => Box[B]): Box[B] = ???
  }
  object Option {
    case class Some[A](a: A) extends Option[A]
    case object None extends Option[Nothing]
  }

}
