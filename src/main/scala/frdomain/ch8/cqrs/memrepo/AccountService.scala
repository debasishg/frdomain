package frdomain.ch8
package cqrs
package memrepo

import java.util.Date
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._
import \/._

import collection.concurrent.TrieMap

import common._

trait Event[+Next] {
  def at: Date
}

case class Opened[Next](no: String, name: String, openingDate: Option[Date], at: Date = today, 
  onInit: Account => Next) extends Event[Next]
case class Closed[Next](no: String, closeDate: Option[Date], at: Date = today, onClose: Account => Next) extends Event[Next]
case class Debited[Next](no: String, amount: Amount, at: Date = today, onDebit: Account => Next) extends Event[Next]
case class Credited[Next](no: String, amount: Amount, at: Date = today, onCredit: Account => Next) extends Event[Next]

object Event {

  val eventLog = TrieMap[String, List[Event[_]]]() 

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

  def snapshot(es: List[Event[_]]): String \/ Map[String, Account] = 
    es.reverse.foldLeft(Map.empty[String, Account]) { (a, e) => updateState(e, a) }.right
}

object Commands extends Commands {
  import Event._

  type Error = String

  def exists(no: String): Error \/ List[Event[_]] = {
    val currentList = eventLog.getOrElse(no, Nil)
    if (currentList.isEmpty) s"Account $no does not exist".left
    else currentList.right
  }

  def closed(a: Account): Error \/ Account =
    if (a.dateOfClosing isDefined) s"Account ${a.no} is closed".left
    else a.right

  def beforeOpeningDate(a: Account, cd: Option[Date]): Error \/ Account =
    if (a.dateOfOpening before cd.getOrElse(today)) 
      s"Cannot close at a date earlier than opening date ${a.dateOfOpening}".left
    else a.right

  def sufficientFundsToDebit(a: Account, amount: Amount): Error \/ Account =
    if (a.balance.amount < amount) s"insufficient fund to debit $amount from ${a.no}".left
    else a.right

  def validateClose(no: String, cd: Option[Date]) = for {
    l <- exists(no)
    s <- snapshot(l)
    a <- closed(s(no))
    _ <- beforeOpeningDate(a, cd)
  } yield s

  def validateDebit(no: String, amount: Amount) = for {
    l <- exists(no)
    s <- snapshot(l)
    a <- closed(s(no))
    _ <- sufficientFundsToDebit(a, amount)
  } yield s

  def validateCredit(no: String) = for {
    l <- exists(no)
    s <- snapshot(l)
    _ <- closed(s(no))
  } yield s
    
  def handleCommand[A](e: Event[A]) = e match {

    case o @ Opened(no, name, odate, _, onInit) => eventLog.get(no)
      .map { _ => fail(new RuntimeException(s"Account with no = $no already exists")) }
      .getOrElse {
        val a = Account(no, name, odate.get)
        eventLog += (no -> List(o))
        now(onInit(a))
      }

    case c @ Closed(no, cdate, _, onClose) => validateClose(no, cdate).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (c :: eventLog.getOrElse(no, Nil)))
        onClose(updateState(c, currentState)(no))
      }
    )

    case d @ Debited(no, amount, _, onDebit) => validateDebit(no, amount).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (d :: eventLog.getOrElse(no, Nil)))
        onDebit(updateState(d, currentState)(no))
      }
    )

    case r @ Credited(no, amount, _, onCredit) => validateCredit(no).fold(
      err => fail(new RuntimeException(err)),
      currentState => now {
        eventLog += (no -> (r :: eventLog.getOrElse(no, Nil)))
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

  def open(no: String, name: String, openingDate: Option[Date]): Command[Account] = Opened(no, name, openingDate, today, identity)
  def close(no: String, closeDate: Option[Date]): Command[Account] = Closed(no, closeDate, today, identity)
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
