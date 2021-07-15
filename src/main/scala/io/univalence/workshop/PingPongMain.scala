package io.univalence.workshop

import actor4fun.{Actor, ActorRef, ActorSystem}
import scala.util.Random

object PingPongMain {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val system = ActorSystem.createLocal("hollywood")
    sys.addShutdownHook(system.shutdown())

    val ping = system.registerAndManage("ping", new PingPongActor("ping", system))
    val pong = system.registerAndManage("pong", new PingPongActor("pong", system))

    pong.sendFrom(ping, Ball)

    system.awaitTermination()
  }

}

case object Ball
case object Miss

class PingPongActor(name: String, system: ActorSystem) extends Actor {
  var exchangeCount = 0

  override def receive(sender: ActorRef)(implicit self: ActorRef): Receive = {
    case Ball =>
      println(s"$exchangeCount: $name")
      Thread.sleep(500)
      if (Random.nextInt(100) < 10)
        sender ! Miss
      else {
        exchangeCount += 1
        sender ! Ball
      }

    case Miss =>
      println(s"$exchangeCount: $name missed!!!!")
      system.shutdown()
  }

}
