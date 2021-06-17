package io.univalence.workshop.old

import scala.annotation.tailrec

sealed trait List[+A] {

  /*
    Cons("Hello", Nil) => length = 1
    Cons("Hello", Cons("Bye", Nil)) => 2
 */
  def length: Int =
    this match {
      case Nil        => 0
      case Cons(_, t) => 1 + t.length
    }

  def isEmpty: Boolean =
    this match {
      case Nil        => true
      case Cons(_, _) => false
    }

  /* Nil.map(_ + 1) => Nil
   * Cons(1, Nil).map(_ + 1) => Cons(2, Nil)
   *
   */
  def map[B](f: A => B): List[B] = {
     this match {
      case Nil        => Nil
      case Cons(h, t) => Cons(f(h), t.map(f))
    }
  }

  def filter(f: A => Boolean): List[A] = {
      this match {
      case Nil        => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, t.filter(f))
        else t.filter(f)
    }
  }

  @tailrec
  final def foldLeft[B](init: B)(f: (B, A) => B): B = {
      this match {
      case Nil        => init
      case Cons(h, t) =>
        t.foldLeft(f(init, h))(f)
    }
  }

  // Nil
  def sum[B >: A](implicit ev: Numeric[B]): B = {
//      this match {
//      case Nil        => ev.zero
//      case Cons(h, t) =>
//        ev.plus(h, t.sum(ev))
//    }
        foldLeft(ev.zero)(ev.plus)
  }

  def flatMap[B](f: A => List[B]): List[B] = {
      ???
  }

}

case object Nil                            extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object ListMain {
  def main(args: Array[String]): Unit = {
    val l =  Cons("Hello", Cons("Bye", Nil))
    println(l.map(_.length))

    val l1 = Cons(1, Cons(2, Cons(3, Nil)))

    println(l1.sum)

    val v1 = { println("hello 1"); 2 }
    lazy val v2 = { println("hello 2"); 3 }
    val v3 = () => { println("hello 3"); 3 }

    println("print values")
    println(v1)
    println(v2)
    println(v2)
    println(v3())
    println(v3())
    display1 { println("hello 4"); "4" }
    display2 { println("hello 5"); "5" }
  }

  def display1(m: String): Unit = {
    println(m)
    println(m)
  }

  def display2(m: => String): Unit = {
    lazy val im = m
    println(im)
    println(im)
  }
}
