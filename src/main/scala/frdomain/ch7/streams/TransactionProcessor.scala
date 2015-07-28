package frdomain.ch7
package streams

import akka.actor.{ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.stream.actor.ActorSubscriber
import akka.stream.io.Framing
import akka.stream.scaladsl.{Tcp, Source, Sink}
import akka.util.ByteString

import scala.concurrent.duration._

class TransactionProcessor(host: String, port: Int)(implicit val system: ActorSystem) extends Logging {

  def run(): Unit = {
    implicit val mat = ActorMaterializer()

    val summarizer = system.actorOf(Props[Summarizer])

    logger.info(s"Receiver: binding to $host:$port")

    Tcp().bind(host, port).runForeach { conn =>
      val receiveSink = 
        conn.flow
            .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 4000, allowTruncation = true)).map(_.utf8String)
            // .transform(() => Stages.parseLines("\n", 4000))
            .map(_.split(","))
            .mapConcat(Transaction(_).toList)
            .to(Sink(ActorSubscriber[Transaction](summarizer)))

      Source.empty.to(receiveSink).run()
    }

    import system.dispatcher
    system.scheduler.schedule(0.seconds, 1.second, summarizer, LogSummaryBalance)
  }
}

object TransactionProcessor extends App {
  implicit val system = ActorSystem("processor")
  new TransactionProcessor("localhost", 9982).run()
}

