package frdomain.ch8
package cqrs
package service

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import \/._

import cqrs.lib._

import common._

case class Opened(no: String, name: String, openingDate: Option[DateTime], at: DateTime = today) extends Event[Account]
case class Closed(no: String, closeDate: Option[DateTime], at: DateTime = today) extends Event[Account]
case class Debited(no: String, amount: Amount, at: DateTime = today) extends Event[Account]
case class Credited(no: String, amount: Amount, at: DateTime = today) extends Event[Account]

object AccountSnapshot extends Snapshot[Account] {

  def updateState(e: Event[_], initial: Map[String, Account]) = e match {
    case o @ Opened(no, name, odate, _) =>
      initial + (no -> Account(no, name, odate.get))

    case c @ Closed(no, cdate, _) =>
      initial + (no -> initial(no).copy(dateOfClosing = Some(cdate.getOrElse(today))))

    case d @ Debited(no, amount, _) =>
      val a = initial(no)
      initial + (no -> a.copy(balance = Balance(a.balance.amount - amount)))

    case r @ Credited(no, amount, _) =>
      val a = initial(no)
      initial + (no -> a.copy(balance = Balance(a.balance.amount + amount)))
  }
}

trait AccountCommands extends Commands[Account] {
  import scala.language.implicitConversions

  private implicit def liftEvent[Next](event: Event[Next]): Command[Next] = Free.liftF(event)

  def open(no: String, name: String, openingDate: Option[DateTime]): Command[Account] = 
    Opened(no, name, openingDate, today)
  def close(no: String, closeDate: Option[DateTime]): Command[Account] = 
    Closed(no, closeDate, today)
  def debit(no: String, amount: Amount): Command[Account] = 
    Debited(no, amount, today)
  def credit(no: String, amount: Amount): Command[Account] = 
    Credited(no, amount, today)
}

object RepositoryBackedAccountInterpreter extends RepositoryBackedInterpreter {
  import AccountSnapshot._
  import spray.json._
  import JSONProtocols._

  // val eventLog = InMemoryEventStore.apply[String]
  val eventLog = new InMemoryJSONEventStore {
    val eventJsonFormat = EventFormat
  }.apply[String]

  import eventLog._

  val step: Event ~> Task = new (Event ~> Task) {
    override def apply[A](action: Event[A]): Task[A] = handleCommand(action)
  }

  private def closed(a: Account): Error \/ Account =
    if (a.dateOfClosing isDefined) s"Account ${a.no} is closed".left
    else a.right

  private def beforeOpeningDate(a: Account, cd: Option[DateTime]): Error \/ Account =
    if (a.dateOfOpening isBefore cd.getOrElse(today)) 
      s"Cannot close at a date earlier than opening date ${a.dateOfOpening}".left
    else a.right

  private def sufficientFundsToDebit(a: Account, amount: Amount): Error \/ Account =
    if (a.balance.amount < amount) s"insufficient fund to debit $amount from ${a.no}".left
    else a.right

  private def validateClose(no: String, cd: Option[DateTime]) = for {
    l <- events(no)
    s <- snapshot(l)
    a <- closed(s(no))
    _ <- beforeOpeningDate(a, cd)
  } yield s

  private def validateDebit(no: String, amount: Amount) = for {
    l <- events(no)
    s <- snapshot(l)
    a <- closed(s(no))
    _ <- sufficientFundsToDebit(a, amount)
  } yield s

  private def validateCredit(no: String) = for {
    l <- events(no)
    s <- snapshot(l)
    _ <- closed(s(no))
  } yield s
    
  def validateOpen(no: String) = {
    val events = eventLog.get(no)
    if (events nonEmpty) s"Account with no = $no already exists".left
    else no.right
  }

  private def handleCommand[A](e: Event[A]): Task[A] = e match {

    case o @ Opened(no, name, odate, _) => Task { 
      validateOpen(no).fold(
        err => throw new RuntimeException(err),
        _   => { 
          val a = Account(no, name, odate.get)
          eventLog.put(no, o)
          a
        }
      )
    }

    case c @ Closed(no, cdate, _) => Task {
      validateClose(no, cdate).fold(
        err => throw new RuntimeException(err),
        currentState => {
          eventLog.put(no, c)
          updateState(c, currentState)(no)
        }
      )
    }

    case d @ Debited(no, amount, _) => Task {
      validateDebit(no, amount).fold(
        err => throw new RuntimeException(err),
        currentState => {
          eventLog.put(no, d)
          updateState(d, currentState)(no)
        }
      )
    }

    case r @ Credited(no, amount, _) => Task {
      validateCredit(no).fold(
        err => throw new RuntimeException(err),
        currentState => {
          eventLog.put(no, r)
          updateState(r, currentState)(no)
        }
      )
    }
  }
}

object Scripts extends AccountCommands {

  def transfer(from: String, to: String, amount: Amount): Command[Unit] = for {
    _ <- debit(from, amount)
    _ <- credit(to, amount)
  } yield ()

  val composite =
    for {
      a <- open("a-123", "debasish ghosh", Some(today))
      _ <- credit(a.no, 10000)
      _ <- credit(a.no, 30000)
      d <- debit(a.no, 23000)
    } yield d

  val compositeFail =
    for {
      a <- open("a-124", "debasish ghosh", Some(today))
      _ <- credit(a.no, 10000)
      _ <- credit(a.no, 30000)
      d <- debit(a.no, 50000)
    } yield d

  val comp =
    for {
      a <- open("a1", "debasish ghosh", Some(today))
      _ <- credit(a.no, 10000)
      _ <- credit(a.no, 30000)
      d <- debit(a.no, 23000)
    } yield d
}
