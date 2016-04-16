package frdomain.ch8
package cqrs
package memrepo

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._

import collection.concurrent.TrieMap

import common._
import spray.json._
import JSONProtocols._

trait Event[A] {
  def at: DateTime
}

case class Opened(no: String, name: String, openingDate: Option[DateTime], at: DateTime = today) extends Event[Account]
case class Closed(no: String, closeDate: Option[DateTime], at: DateTime = today) extends Event[Account]
case class Debited(no: String, amount: Amount, at: DateTime = today) extends Event[Account]
case class Credited(no: String, amount: Amount, at: DateTime = today) extends Event[Account]

object Event {

  val eventLog = TrieMap[String, List[Event[_]]]() 
  val eventLogJson = TrieMap[String, List[String]]()

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

  def events(no: String): Error \/ List[Event[_]] = {
    val currentList = eventLog.getOrElse(no, Nil)
    if (currentList.isEmpty) s"Account $no does not exist".left
    else currentList.right
  }

  def snapshot(es: List[Event[_]]): String \/ Map[String, Account] = 
    es.reverse.foldLeft(Map.empty[String, Account]) { (a, e) => updateState(e, a) }.right

  def snapshotFromJson(es: List[String]): String \/ Map[String, Account] = 
    es.reverse.foldLeft(Map.empty[String, Account]) { (a, e) => updateState(e.parseJson.convertTo[Event[_]], a) }.right
}

object Commands extends Commands {
  import Event._

  def closed(a: Account): Error \/ Account =
    if (a.dateOfClosing isDefined) s"Account ${a.no} is closed".left
    else a.right

  def beforeOpeningDate(a: Account, cd: Option[DateTime]): Error \/ Account =
    if (a.dateOfOpening isBefore cd.getOrElse(today)) 
      s"Cannot close at a date earlier than opening date ${a.dateOfOpening}".left
    else a.right

  def sufficientFundsToDebit(a: Account, amount: Amount): Error \/ Account =
    if (a.balance.amount < amount) s"insufficient fund to debit $amount from ${a.no}".left
    else a.right

  def validateClose(no: String, cd: Option[DateTime]) = for {
    l <- events(no)
    s <- snapshot(l)
    a <- closed(s(no))
    _ <- beforeOpeningDate(a, cd)
  } yield s

  def validateDebit(no: String, amount: Amount) = for {
    l <- events(no)
    s <- snapshot(l)
    a <- closed(s(no))
    _ <- sufficientFundsToDebit(a, amount)
  } yield s

  def validateCredit(no: String) = for {
    l <- events(no)
    s <- snapshot(l)
    _ <- closed(s(no))
  } yield s

  def validateOpen(no: String) =
    eventLog.get(no)
            .map { _ => s"Account with no = $no already exists".left }
            .getOrElse(no.right)
    
    
  def handleCommand[A](e: Event[A]): Task[A] = e match {

    case o @ Opened(no, name, odate, _) => validateOpen(no).fold(
      err => fail(new RuntimeException(err)),
      _   => now {
        val a = Account(no, name, odate.get)
        eventLog += (no -> List(o))
        eventLogJson += (no -> List(OpenedFormat.write(o).toString))
        a
      }
    )

    case c @ Closed(no, cdate, _) => validateClose(no, cdate).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (c :: eventLog.getOrElse(no, Nil)))
        eventLogJson += (no -> (ClosedFormat.write(c).toString :: eventLogJson.getOrElse(no, Nil)))
        updateState(c, currentState)(no)
      }
    )

    case d @ Debited(no, amount, _) => validateDebit(no, amount).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (d :: eventLog.getOrElse(no, Nil)))
        eventLogJson += (no -> (DebitedFormat.write(d).toString :: eventLogJson.getOrElse(no, Nil)))
        updateState(d, currentState)(no)
      }
    )

    case r @ Credited(no, amount, _) => validateCredit(no).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (r :: eventLog.getOrElse(no, Nil)))
        eventLogJson += (no -> (CreditedFormat.write(r).toString :: eventLogJson.getOrElse(no, Nil)))
        updateState(r, currentState)(no)
      }
    )
  }
}

trait Commands {
  import Event._
  import scala.language.implicitConversions

  type Command[A] = Free[Event, A]

  private implicit def liftEvent[Next](event: Event[Next]): Command[Next] = Free.liftF(event)

  def open(no: String, name: String, openingDate: Option[DateTime]): Command[Account] = Opened(no, name, openingDate, today)
  def close(no: String, closeDate: Option[DateTime]): Command[Account] = Closed(no, closeDate, today)
  def debit(no: String, amount: Amount): Command[Account] = Debited(no, amount, today)
  def credit(no: String, amount: Amount): Command[Account] = Credited(no, amount, today)
}

object RepositoryBackedInterpreter {
  import Commands._

  val step: Event ~> Task = new (Event ~> Task) {
    override def apply[A](action: Event[A]): Task[A] = handleCommand(action)
  }

  def apply[A](action: Command[A]): Task[A] = action.foldMap(step)
}

object Scripts extends Commands {

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

object Projections {
  import Event._

  def balance(no: String) = for {
    l <- events(no)
    s <- snapshot(l)
  } yield s(no).balance

}
