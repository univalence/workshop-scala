package io.univalence.workshop

import scala.annotation.tailrec

sealed trait Natural
object Natural {
  case object Zero               extends Natural
  case class Succ(prev: Natural) extends Natural

  def fromInt(i: Int): Natural = {
    require(i >= 0)

    if (i == 0)
      Zero
    else
      Succ(fromInt(i - 1))
  }

  def toInt(n: Natural): Int =
    n match {
      case Zero    => 0
      case Succ(p) => 1 + toInt(p)
    }

  // (x + 1) + y == x + (y + 1)
  // n1 + n2
  // n1 = p + 1
  // (p + 1) + n2 = p + (n2 + 1)
  @tailrec
  def add(n1: Natural, n2: Natural): Natural = {
    n1 match {
      case Zero    => n2
      case Succ(p) => add(p, Succ(n2))
    }
  }

  // (p + 1) * n2 = p * n2 + n2
  def mult(n1: Natural, n2: Natural): Natural = {
    n1 match {
      case Zero    => Zero
      case Succ(p) => add(n2, mult(p, n2))
    }
  }

  // 3 ^ 2  => 9
  // 0 ^ 0 => 1
  // 0 ^ ? => 0
  // ? ^ 0 => 1
  // n1 ^ n2 = n1 * (n1 ^ (n2 - 1)) = n1 * n1 * (n1 ^ (n2 - 2))
  def pow(n1: Natural, n2: Natural): Natural = {
    (n1, n2) match {
      case (_, Zero)     => Succ(Zero)
      case (Zero, _)     => Zero
      case (_, Succ(p2)) => mult(n1, pow(n1, p2))
    }
  }

  //5 - 6 = None
  //5 - 2 = Some(3)
  // (1 + p1) - (1 + p2) =>
  @tailrec
  def substract(n1: Natural, n2: Natural): Option[Natural] = {
    (n1, n2) match {
      case (Zero, Zero)         => Some(Zero)
      case (Zero, Succ(_))      => None
      case (Succ(_), Zero)      => Some(n1)
      case (Succ(p1), Succ(p2)) => substract(p1, p2)
    }
  }

  @tailrec
  def gt(n1: Natural, n2: Natural): Boolean =
    (n1, n2) match {
      case (Zero, _)            => false
      case (Succ(_), Zero)      => true
      case (Succ(p1), Succ(p2)) => gt(p1, p2)
    }

  //
  case class Divide(quotient: Natural, rest: Natural)

  // n1 == Zero   => Zero
  // n2 == Succ(Zero) => n1
  // n1 == n2 =>
  // 6/2 => Succ(p) Divide(, ) => Divide(3, Zero)
  // n1 / n2 => n2 * q + r = n1
  def divide(n1: Natural, n2: Succ): Divide =
    div(n1, n2, Zero, n1)

  // div(n1, n2, 0, n1)
  @tailrec
  private def div(
      n1: Natural,
      n2: Succ,
      count: Natural,
      rest: Natural
  ): Divide =
    if (gt(n2, rest))
      Divide(count, rest)
    else
      div(n1, n2, Succ(count), substract(rest, n2).get)

}

object Prg {
  import Natural._

  val zero = Natural.Zero
  val un   = Natural.Succ(zero)
  val deux = Natural.Succ(un)

  def main(args: Array[String]): Unit = {
    val result1 = divide(fromInt(6), deux)
    println(s"Divide(${toInt(result1.quotient)}, ${toInt(result1.rest)})")

    val result2 = divide(fromInt(11), Succ(deux))
    println(s"Divide(${toInt(result2.quotient)}, ${toInt(result2.rest)})")

    val result3 = divide(Succ(deux), Succ(deux))
    println(s"Divide(${toInt(result3.quotient)}, ${toInt(result3.rest)})")

  val result4 = pow(deux, Succ(deux))
    println(s"${toInt(result4)}")
  }
}

// flatMap: (M[A], f: A => M[B]) => M[B]

// Spark
// flatMap: (RDD[A], f: A => Iterable[B]) => RDD[B]

// Kafka Streams
// flatMap: (KStream[K, A], f: (K, A) => Iterable[(L, B)]) => KStream[L, B]

// def flatMap[B](transformer: A => MyList[B]): MyList[B]
