package frdomain.ch7
package streams

import akka.actor.ActorSystem
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString

import scala.concurrent.duration._

object Sender extends App with Logging {
  implicit val system = ActorSystem("sender")
  val serverConnection = Tcp().outgoingConnection("localhost", 9982)

  val path = "/Users/debasishghosh/projects/frdomain/src/main/resources/transactions.csv"
  val getLines = () => scala.io.Source.fromFile(path).getLines()

  val linesSource = Source(getLines).map { line => ByteString(line + "\n") }
  val logCompleteSink = Sink.onComplete(r => logger.info("Completed with: " + r))

  val graph = FlowGraph.closed() { implicit b =>
    import FlowGraph.Implicits._

    val broadcast = b.add(Broadcast[ByteString](2))

    val logWindowFlow = Flow[ByteString]
      .groupedWithin(10000, 1.seconds)
      .map(group => group.map(_.size).foldLeft(0)(_ + _))
      .map(groupSize => logger.info(s"Sent $groupSize bytes"))

    linesSource ~> broadcast ~> serverConnection ~> logCompleteSink
                   broadcast ~> logWindowFlow    ~> Sink.ignore
  }

  implicit val mat = ActorFlowMaterializer()
  graph.run()
}


