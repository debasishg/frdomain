package frdomain.ch6
package streams

import scalaz.{ Source => Sourcez, Sink => Sinkz, _ }
import Scalaz._

import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.stream._
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.concurrent.Future


import common._
import OnlineService._
import Transaction._

object Main {
  implicit val as = ActorSystem()
  implicit val ec = as.dispatcher
  val settings = ActorMaterializerSettings(as)
  implicit val mat = ActorMaterializer(settings)

  /**
   * Create a stream of transactions
   */
  val transactions: Source[Transaction, akka.NotUsed] =
    Source.fromFuture(allTransactions).mapConcat(identity)

  val accountNos: Source[String, akka.NotUsed] =
    Source.fromFuture(allAccounts).mapConcat(identity)

  /**
   * Would like to fold transactions through monoid append
   */
  val txnSink: Sink[Transaction, Future[Transaction]] =
    Sink.fold[Transaction, Transaction](TransactionMonoid.zero)(_ |+| _)

  val netTxnSink: Sink[Transaction, Future[Map[String, Transaction]]] = {
    Sink.fold[Map[String, Transaction], Transaction](Map.empty[String, Transaction]) { (acc, t) => acc |+| Map(t.accountNo -> t) }
  }

  /**
   * Dummy function for writing transactions
   */
  val audit: Sink[Transaction, Future[akka.Done]] = Sink.foreach(println)
  val writeNetAll: Sink[Map[String, Transaction], Future[akka.Done]] = Sink.foreach(println)

  /**
   * Create multiple streams out of a single stream. The stream "transactions" is being
   * demultiplexed into many streams split by account number. Each of the sub-streams are
   * then materialized to the fold sink "txnSink", which folds each of the transaction
   * substreams to compute the net value of the transaction for that account
   */
  val netTxn: RunnableGraph[Future[Transaction]] = 
    transactions.map(validate).groupBy(100, _.accountNo).mergeSubstreams.toMat(txnSink)(Keep.right) 

  /**
   * Run all the materialized streams and print
   */
  // netTxn.run().foreach(println)

  val graph = RunnableGraph.fromGraph(GraphDSL.create(netTxnSink) { implicit b => ms =>
    import GraphDSL.Implicits._
 
    val accountBroadcast = b.add(Broadcast[Account](2))
    val txnBroadcast = b.add(Broadcast[Transaction](2))
    val merge = b.add(Merge[Transaction](2))

    val accounts = Flow[String].map(queryAccount(_, AccountRepository))
    val bankingTxns = Flow[Account].mapConcat(getBankingTransactions)
    val settlementTxns = Flow[Account].mapConcat(getSettlementTransactions)
    val validation = Flow[Transaction].map(validate)

    accountNos ~> accounts ~> accountBroadcast ~> bankingTxns ~> merge ~> validation ~> txnBroadcast ~> ms
                              accountBroadcast ~> settlementTxns ~> merge
    txnBroadcast ~> audit
    ClosedShape
  })

  // val r = graph.run()
  // r foreach println
}
