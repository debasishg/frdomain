package frdomain.ch8
package cqrs
package service

import java.util.Date
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._
import \/._

import collection.concurrent.TrieMap
import lib._

import common._

case class Opened[Next](no: String, name: String, openingDate: Option[Date], at: Date = today, 
  onInit: Account => Next) extends Event[Next]
case class Closed[Next](no: String, closeDate: Option[Date], at: Date = today, onClose: Account => Next) extends Event[Next]
case class Debited[Next](no: String, amount: Amount, at: Date = today, onDebit: Account => Next) extends Event[Next]
case class Credited[Next](no: String, amount: Amount, at: Date = today, onCredit: Account => Next) extends Event[Next]

object Event {

  implicit def functor: Functor[Event] = new Functor[Event] {
    override def map[A, B](fa: Event[A])(f: (A) => B): Event[B] = fa match {
      case o @ Opened(no, nm, odt, _, onInit) => o.copy(onInit = onInit andThen f)
      case c @ Closed(no, cdt, _, onClose) => c.copy(onClose = onClose andThen f)
      case d @ Debited(no, amt, _, onDebit) => d.copy(onDebit = onDebit andThen f)
      case r @ Credited(no, amt, _, onCredit) => r.copy(onCredit = onCredit andThen f)
    }
  }
}

object AccountSnapshot extends Snapshot[Account] {

  def updateState(e: Event[_], initial: Map[String, Account]) = e match {
    case o @ Opened(no, name, odate, _, _) =>
      initial + (no -> Account(no, name, odate.get))

    case c @ Closed(no, cdate, _, next) => 
      initial + (no -> initial(no).copy(dateOfClosing = Some(cdate.getOrElse(today))))

    case d @ Debited(no, amount, _, next) => 
      val a = initial(no)
      initial + (no -> a.copy(balance = Balance(a.balance.amount - amount)))

    case r @ Credited(no, amount, _, next) => 
      val a = initial(no)
      initial + (no -> a.copy(balance = Balance(a.balance.amount + amount)))
  }
}

trait AccountCommands extends Commands[Account] {
  import Event._
  import scala.language.implicitConversions

  private implicit def liftEvent[Next](event: Event[Next]): Command[Next] = liftF(event)

  def open(no: String, name: String, openingDate: Option[Date]): Command[Account] = Opened(no, name, openingDate, today, identity)
  def close(no: String, closeDate: Option[Date]): Command[Account] = Closed(no, closeDate, today, identity)
  def debit(no: String, amount: Amount): Command[Account] = Debited(no, amount, today, identity)
  def credit(no: String, amount: Amount): Command[Account] = Credited(no, amount, today, identity)
}

object RepositoryBackedAccountInterpreter extends RepositoryBackedInterpreter {
  import Event._
  import AccountSnapshot._

  val eventLog = InMemoryEventStore.apply[String]

  import eventLog._

  def step[A](action: Event[Free[Event, A]]): Task[Free[Event, A]] = handleCommand(action)

  private def closed(a: Account): Error \/ Account =
    if (a.dateOfClosing isDefined) s"Account ${a.no} is closed".left
    else a.right

  private def beforeOpeningDate(a: Account, cd: Option[Date]): Error \/ Account =
    if (a.dateOfOpening before cd.getOrElse(today)) 
      s"Cannot close at a date earlier than opening date ${a.dateOfOpening}".left
    else a.right

  private def sufficientFundsToDebit(a: Account, amount: Amount): Error \/ Account =
    if (a.balance.amount < amount) s"insufficient fund to debit $amount from ${a.no}".left
    else a.right

  private def validateClose(no: String, cd: Option[Date]) = for {
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
    
  private def handleCommand[A](e: Event[A]) = e match {

    case o @ Opened(no, name, odate, _, onInit) => 
      val events = eventLog.get(no)
      if (events isEmpty) {
        val a = Account(no, name, odate.get)
        eventLog.put(no, o)
        now(onInit(a))
      } else fail(new RuntimeException(s"Account with no = $no already exists"))

    case c @ Closed(no, cdate, _, onClose) => validateClose(no, cdate).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog.put(no, c)
        onClose(updateState(c, currentState)(no))
      }
    )

    case d @ Debited(no, amount, _, onDebit) => validateDebit(no, amount).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog.put(no, d)
        onDebit(updateState(d, currentState)(no))
      }
    )

    case r @ Credited(no, amount, _, onCredit) => validateCredit(no).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog.put(no, r)
        onCredit(updateState(r, currentState)(no))
      }
    )
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
}

/*
object Projections {
  import EventLog._
  import AccountSnapshot._

  def balance(no: String) = for {
    l <- events(no)
    s <- snapshot(l)
  } yield s(no).balance
}
*/
