package io.univalence.workshop

import java.time.Instant
import scala.util.Random

object Arbitrary {

  trait Arbitrary[+A] {

    def unsafeChoose: A

    def map[B](f: A => B): Arbitrary[B] = Arbitrary(f(unsafeChoose))

    def flatMap[B](f: A => Arbitrary[B]): Arbitrary[B] =
      Arbitrary {
        f(unsafeChoose).unsafeChoose
      }

  }

  object Arbitrary {
    def apply[A](a: => A): Arbitrary[A] =
      new Arbitrary[A] {
        override def unsafeChoose: A = a
      }

    def int(max: Int): Arbitrary[Int] =
      Arbitrary {
        Random.nextInt(max)
      }

    val double: Arbitrary[Double] = Arbitrary(Random.nextDouble)

    def string(length: Int): Arbitrary[String] =
      Arbitrary {
        List
          .fill(length) {
            Random.nextPrintableChar()
          }
          .mkString
      }

    def stringOfMax(maxLength: Int): Arbitrary[String] =
      for {
        length <- int(maxLength)
        str    <- string(length)
      } yield str

    val instant: Arbitrary[Instant] = Arbitrary(
      Instant.ofEpochMilli(Random.nextLong)
    )

    def collect[A](arbList: List[Arbitrary[A]]): Arbitrary[List[A]] =
      Arbitrary {
        arbList.map(a => a.unsafeChoose)
      }

    // listOf(5, Arbitrary.int(100)).unsafeChoose => List(64, 22, 1, 45, 42)
    def listOf[A](size: Int, arb: Arbitrary[A]): Arbitrary[List[A]] =
      collect(List.fill(size)(arb))

    // List[Arbitrary[A]] => Arbitrary[List[A]] -> sequence / collect

  }

  def main(args: Array[String]): Unit = {
    import Show._

    val randIntList =
      Arbitrary.listOf(5, stockGenerator)

    randIntList
      .unsafeChoose
      .map(_.display)
      .foreach(println)
  }

  def stockGenerator: Arbitrary[Stock] =
    for {
      pid <- Arbitrary.string(5)
      sid <- Arbitrary.string(5)
      ts  <- Arbitrary.instant
      q   <- Arbitrary.double
    } yield Stock(pid, sid, ts, q)
}

case class Stock(
    productId: String,
    storeId: String,
    timestamp: Instant,
    quantity: Double
)

trait Show[A] {
  def display(a: A): String
}
object Show {
  @inline def apply[A: Show]: Show[A] = implicitly

  implicit val intShow: Show[Int] = (a: Int) => a.toString

  implicit val stockShow: Show[Stock] =
    (a: Stock) => s"Stock(productId=${a.productId}, storeId=${a.storeId}, timestamp=${a.timestamp}, quantity=${a.quantity})"

  implicit class ShowOps[A: Show](a: A) {
    def display: String = Show[A].display(a)
  }
}

trait Combinable[A] {
  def combine(l: A, r: A): A
}
