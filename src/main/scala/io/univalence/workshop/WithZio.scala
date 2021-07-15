package io.univalence.workshop

import zio.console.Console
import zio.{ExitCode, Fiber, URIO, ZIO, console}

object WithZio {

  def main(args: Array[String]): Unit = {
    println(program.fork)
  }

    val program: URIO[Console, Fiber.Runtime[Nothing, ExitCode]] = (for {
      f1 <- console.putStrLn("hello").fork
      f2 <- console.putStrLn("world").fork
      _ <- f1.zip(f2).join
    } yield ())
      .exitCode.fork
}
