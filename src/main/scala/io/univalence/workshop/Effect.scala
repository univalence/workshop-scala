package io.univalence.workshop

import io.univalence.workshop.Effect.Effect.{FlatMap, Run, localNow, putStrLn, unsafeRun}
import java.time.LocalDateTime
import scala.annotation.tailrec

// ADT : Algebraic Data Type
// GADT: Generalized Algebraic Data Type
object Effect {

  // interface java 8
  trait Effect[A] {

    def map[B](f: A => B): Effect[B] =
      flatMap(a => Run(() => f(a)))

    def flatMap[B](f: A => Effect[B]): Effect[B] =
      FlatMap(this, f)
  }

  // classe purement statique et instance / singleton
  object Effect {

    case class Run[A](process: () => A)                       extends Effect[A]
    case class FlatMap[A, B](e: Effect[A], f: A => Effect[B]) extends Effect[B]

    // lancer le traitement associé reccup valeur
    @tailrec
    def unsafeRun[A](e: Effect[A]): A = e match {
        case Run(process) => process()
//        case FlatMap(e1: Effect[Any], f: (Any => Effect[A])) => unsafeRun(f(unsafeRun(e1)))
        case FlatMap(e1: Effect[Any], f: (Any => Effect[A])) =>
          e1 match {
            case Run(process1) => unsafeRun(f(process1()))
            case FlatMap(e2, g: (Any => Effect[Any])) =>
              // FlatMap(FlatMap(e2, g), f)
              // => associativity
              // FlatMap(e2, x => FlatMap(g(x), f))
              unsafeRun(FlatMap(e2, (x: Any) => FlatMap(g(x), f)))
          }
    }

    def putStrLn(message: String): Effect[Unit] = Run(() => println(message))

    def localNow: Effect[LocalDateTime] = Run(() => LocalDateTime.now())

  }

  def main(args: Array[String]): Unit = {
    val value = putStrLn("hello")
    //    println("hello")
//    val date = LocalDateTime.now()
//    println(date)
    lazy val program: Effect[Unit] =
      for {
        _    <- value
        _    <- value
//        _    <- Run[Unit] { throw new RuntimeException("WAAZAAAAAAAA") }
        date <- localNow
        _    <- putStrLn(date.toString)
//        _    <- program
      } yield ()

    println(program)

    unsafeRun(program)
  }

}
