package frdomain.ch7
package streams

import akka.actor.{ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.stream.actor.ActorSubscriber
import akka.stream.scaladsl.{Tcp, Source, Sink, Framing}
import akka.util.ByteString

import scala.concurrent.duration._

/**
 * The TransactionProcessor that gets streaming input from FrontOffice, parses
 * the String and builds the Transaction object. The Summarizer actor reports the updated Balance
 * as it gets the transaction info.
 *
 * Run this as:
 *
 * $ sbt
 * > console
 * scala> import frdomain.ch7.streams._
 * scala> TransactionProcessor.main(Array(""))
 */
class TransactionProcessor(host: String, port: Int)(implicit val system: ActorSystem) extends Logging {

  def run(): Unit = {
    implicit val mat = ActorMaterializer()

    val summarizer = system.actorOf(Props[Summarizer])

    logger.info(s"Receiver: binding to $host:$port")

    Tcp().bind(host, port).runForeach { conn =>
      val receiveSink = 
        conn.flow
            .via(Framing.delimiter(ByteString(System.lineSeparator), maximumFrameLength = 4000, allowTruncation = true))
            .map(_.utf8String)
            .map(_.split(","))
            .mapConcat(Transaction(_).toList)
            .to(Sink.fromSubscriber(ActorSubscriber[Transaction](summarizer)))

      receiveSink.runWith(Source.maybe)
    }

    import system.dispatcher
    system.scheduler.schedule(0.seconds, 5.seconds, summarizer, LogSummaryBalance)
  }
}

object TransactionProcessor extends App {
  implicit val system = ActorSystem("processor")
  new TransactionProcessor("127.0.0.1", 9982).run()
}

