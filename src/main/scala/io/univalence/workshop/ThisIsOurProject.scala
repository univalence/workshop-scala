package io.univalence.workshop

import scala.annotation.tailrec

object Natural2 {

  sealed trait Natural
  case object Zero            extends Natural
  case class Succ(n: Natural) extends Natural

  def int2Natural(i: Int): Natural =
    if (i == 0) Zero
    else Succ(int2Natural(i - 1))

  @tailrec
  def natural2Int(n: Natural, result: Int = 0): Int =
    n match {
      case Zero    => result
      case Succ(s) => natural2Int(s, result + 1)
    }

  /* Succ(Succ(Zero)) + Succ(Zero)
   * 2: Succ(Zero)) + Succ(Succ(Zero)))
   * 2: Zero + Succ(Succ(Succ(Zero))))
   * 1: fini => Succ(Succ(Succ(Zero))))
   */
  @tailrec
  def add(n1: Natural, n2: Natural): Natural =
    n1 match {
      case Zero    => n2
      case Succ(s) => add(s, Succ(n2))
    }

  /**
    * (3 * 2) => 2 + (2 * 2) => 2 + 2 + (1 * 2)
    */
  def mult(n1: Natural, n2: Natural): Natural =
    n1 match {
      case Zero    => Zero
      case Succ(s) => add(n2, mult(s, n2))
    }

  def subs(n1: Natural, n2: Natural): Option[Natural] =
    (n1, n2) match {
      case (_, Zero)            => Some(n1)
      case (Zero, _)            => None
      case (Succ(s1), Succ(s2)) => subs(s1, s2)
    }

  // (q, m) => n1 = q * n2 + m
  def div(n1: Natural, n2: Succ, q: Natural = Zero): (Natural, Natural) =
    n2 match {
      case Succ(Zero) => (n1, q)
      case Succ(_) =>
        subs(n1, n2) match {
          case None      => (q, n1)
          case Some(sub) => div(sub, n2, Succ(q))
        }
    }

  sealed trait List[+A] {
    def size(acc: Int = 0): Int =
      this match {
        case Nil        => acc
        case Cons(_, t) => t.size(acc + 1)
      }

    def map[B](f: A => B): List[B] =
      this match {
        case Nil        => Nil
        case Cons(a, t) => Cons(f(a), t.map(f))
      }

    def concat[B >: A](l2: List[B]): List[B] =
      this match {
        case Nil        => l2
        case Cons(h, t) => Cons(h, t.concat(l2))
      }

    def flatMap[B](f: A => List[B]): List[B] =
      this match {
        case Nil        => Nil
        case Cons(a, t) => f(a).concat(t.flatMap(f))
      }

    def map2[B](f: A => B): List[B] =
      flatMap(a => Cons(f(a), Nil))
  }

  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case object Nil                            extends List[Nothing]

  def main(args: Array[String]): Unit = {
//    println(int2Natural(3))
//    println(natural2Int(Succ(Succ(Succ(Zero)))))

//    println(natural2Int(mult(int2Natural(5), int2Natural(4))))
//    println(subs(int2Natural(5), int2Natural(4)).map(n => natural2Int(n)))
//
//    println(div(int2Natural(5), int2Natural(4).asInstanceOf[Succ]))
//    println(div(int2Natural(4), int2Natural(5).asInstanceOf[Succ]))
//    println(div(int2Natural(4), int2Natural(2).asInstanceOf[Succ]))
//    println(div(int2Natural(6), int2Natural(4).asInstanceOf[Succ]))

    val list1 = Cons(1, Cons(2, Cons(3, Nil)))
    val list2 = Cons(4, Cons(5, Nil))

    println(list1.size())
    println(list1.map(_ + 1))

    println(
      list1.flatMap(e1 => list2.map(e2 => (e1, e2, e1 + e2)))
    )
  }
}
