package frdomain.ch7
package streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString

import scala.concurrent.duration._

object FrontOffice extends App with Logging {
  implicit val system = ActorSystem("front_office")
  val serverConnection = Tcp().outgoingConnection("localhost", 9982)

  val path = "/Users/debasishghosh/projects/frdomain/src/main/resources/transactions.csv"
  val getLines = () => scala.io.Source.fromFile(path).getLines()

  val readLines = Source(getLines).filter(isValid).map( l => ByteString(l + "\n"))

  def isValid(line: String) = true

  val logWhenComplete = Sink.onComplete(r => logger.info("Transfer complete: " + r))

  val graph = FlowGraph.closed() { implicit b =>
    import FlowGraph.Implicits._

    val broadcast = b.add(Broadcast[ByteString](2))

    val heartbeat = Flow[ByteString]
      .groupedWithin(10000, 1.seconds)
      .map(_.map(_.size).foldLeft(0)(_ + _))
      .map(groupSize => logger.info(s"Sent $groupSize bytes"))

    readLines ~> broadcast ~> serverConnection ~> logWhenComplete
                 broadcast ~> heartbeat        ~> Sink.ignore
  }

  implicit val mat = ActorMaterializer()
  graph.run()
}


