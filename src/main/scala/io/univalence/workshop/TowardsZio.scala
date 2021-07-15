package io.univalence.workshop

import io.univalence.workshop.Effect.Effect
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object TowardsZio {

  type EitherT[F[_], A, B] = F[Either[A, B]]

//   F: * -> *
  // * -> * -> * ~ (*, *) -> * => Either, Tuple2 (1, ""), A => B

  val e1: Effect[Either[Exception, Option[String]]]  = ???
  val e2: EitherT[Effect, Exception, Option[String]] = ???

  trait BiEffect[+E, +A] {}

  type ZIO[-R, +E, +A]
  type Task[A]    = ZIO[Any, Throwable, A]
  type IO[E, A]   = ZIO[Any, E, A]
  type RIO[R, A]  = ZIO[R, Throwable, A]
  type UIO[A]     = ZIO[Any, Nothing, A]
  type URIO[R, A] = ZIO[R, Nothing, A]

  def main(args: Array[String]): Unit = {
    val hello: Future[String] =
      Future {
        Thread.sleep(1000)
        throw new Exception
        "Hello"
      }

    println(hello.value)

    val world: Future[String] =
      Future {
        Thread.sleep(1000)
        "world"
      }

    val result: Future[String] =
      for {
        s1 <- hello
        s2 <- world
      } yield s"$s1 $s2"

    Thread.sleep(2000)
    println(result.value)
//    println(Await.result(result, Duration.Inf))
  }

}
