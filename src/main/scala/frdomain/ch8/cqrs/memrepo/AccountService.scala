package frdomain.ch8
package cqrs
package memrepo

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._
import \/._

import collection.concurrent.TrieMap

import common._
import spray.json._
import JSONProtocols._

trait Event[+Next] {
  def at: DateTime
}

case class Opened[Next](no: String, name: String, openingDate: Option[DateTime], at: DateTime = today, 
  onInit: Account => Next) extends Event[Next]
case class Closed[Next](no: String, closeDate: Option[DateTime], at: DateTime = today, 
  onClose: Account => Next) extends Event[Next]
case class Debited[Next](no: String, amount: Amount, at: DateTime = today, onDebit: Account => Next) extends Event[Next]
case class Credited[Next](no: String, amount: Amount, at: DateTime = today, onCredit: Account => Next) extends Event[Next]

object Event {

  val eventLog = TrieMap[String, List[Event[_]]]() 
  val eventLogJson = TrieMap[String, List[String]]()

  implicit def functor: Functor[Event] = new Functor[Event] {
    override def map[A, B](fa: Event[A])(f: (A) => B): Event[B] = fa match {
      case o @ Opened(no, nm, odt, _, onInit) => o.copy(onInit = onInit andThen f)
      case c @ Closed(no, cdt, _, onClose) => c.copy(onClose = onClose andThen f)
      case d @ Debited(no, amt, _, onDebit) => d.copy(onDebit = onDebit andThen f)
      case r @ Credited(no, amt, _, onCredit) => r.copy(onCredit = onCredit andThen f)
    }
  }

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
    
    
  def handleCommand[A](e: Event[A]) = e match {

    case o @ Opened(no, name, odate, _, onInit) => validateOpen(no).fold(
      err => fail(new RuntimeException(err)),
      _   => now {
        val a = Account(no, name, odate.get)
        eventLog += (no -> List(o))
        eventLogJson += (no -> List(OpenedFormat.write(o).toString))
        onInit(a)
      }
    )

    case c @ Closed(no, cdate, _, onClose) => validateClose(no, cdate).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (c :: eventLog.getOrElse(no, Nil)))
        eventLogJson += (no -> (ClosedFormat.write(c).toString :: eventLogJson.getOrElse(no, Nil)))
        onClose(updateState(c, currentState)(no))
      }
    )

    case d @ Debited(no, amount, _, onDebit) => validateDebit(no, amount).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (d :: eventLog.getOrElse(no, Nil)))
        eventLogJson += (no -> (DebitedFormat.write(d).toString :: eventLogJson.getOrElse(no, Nil)))
        onDebit(updateState(d, currentState)(no))
      }
    )

    case r @ Credited(no, amount, _, onCredit) => validateCredit(no).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (r :: eventLog.getOrElse(no, Nil)))
        eventLogJson += (no -> (CreditedFormat.write(r).toString :: eventLogJson.getOrElse(no, Nil)))
        onCredit(updateState(r, currentState)(no))
      }
    )
  }
}

trait Commands {
  import Event._
  import scala.language.implicitConversions

  type Command[A] = Free[Event, A]

  private implicit def liftEvent[Next](event: Event[Next]): Command[Next] = liftF(event)

  def open(no: String, name: String, openingDate: Option[DateTime]): Command[Account] = Opened(no, name, openingDate, today, identity)
  def close(no: String, closeDate: Option[DateTime]): Command[Account] = Closed(no, closeDate, today, identity)
  def debit(no: String, amount: Amount): Command[Account] = Debited(no, amount, today, identity)
  def credit(no: String, amount: Amount): Command[Account] = Credited(no, amount, today, identity)
}

object RepositoryBackedInterpreter {
  import Commands._

  def step[A](action: Event[Free[Event, A]]): Task[Free[Event, A]] = handleCommand(action)
 
  def apply[A](action: Free[Event, A]): Task[A] = action.runM(step)
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
