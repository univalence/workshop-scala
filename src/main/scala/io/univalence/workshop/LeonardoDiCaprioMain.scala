package io.univalence.workshop

import actor4fun.{Actor, ActorRef, ActorSystem}

object LeonardoDiCaprioMain {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val system = ActorSystem.createLocal("hollywood")
    sys.addShutdownHook(system.shutdown())

    val helloActor = new HelloActor
    val ref = system.registerAndManage("hello", helloActor)

    ref.sendFrom(null, "Clint Eastwood")

    system.awaitTermination()
  }

}

class HelloActor extends Actor {

  override def receive(sender: ActorRef)(implicit self: ActorRef): Receive = {
    case message =>
      println(s"Hello $message")
  }

}
