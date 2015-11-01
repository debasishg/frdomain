package frdomain.ch8
package cqrs
package pure

import java.util.Date
import scalaz.{State, Free, Functor}
import scalaz.Free.liftF

import collection.concurrent.TrieMap

import common._

trait Event[+Next]
case class Opened[Next](no: String, name: String, openingDate: Option[Date], onInit: String => Next) extends Event[Next]
case class Closed[Next](no: String, closeDate: Option[Date], next: Next) extends Event[Next]
case class Debited[Next](no: String, amount: Amount, next: Next) extends Event[Next]
case class Credited[Next](no: String, amount: Amount, next: Next) extends Event[Next]

object Event {

  implicit def functor: Functor[Event] = new Functor[Event] {
    override def map[A, B](fa: Event[A])(f: (A) => B): Event[B] = fa match {
      case o @ Opened(no, nm, odt, onInit) => o.copy(onInit = onInit andThen f)
      case c @ Closed(no, cdt, next) => c.copy(next = f(next))
      case d @ Debited(no, amt, next) => d.copy(next = f(next))
      case r @ Credited(no, amt, next) => r.copy(next = f(next))
    }
  }

  private def debitImpl(a: Account, amount: Amount) = {
    if (a.balance.amount < amount) throw new RuntimeException("insufficient fund to debit")
    a.copy(balance = Balance(a.balance.amount - amount))
  }

  private def creditImpl(a: Account, amount: Amount) = {
    a.copy(balance = Balance(a.balance.amount + amount))
  }

  def updateState(e: Event[_], initial: Map[String, Account]) = e match {
    case o @ Opened(no, name, odate, _) =>
      initial + (no -> Account(no, name, odate.get))

    case c @ Closed(no, cdate, next) => 
      initial + (no -> initial(no).copy(dateOfClosing = cdate))

    case d @ Debited(no, amount, next) => 
      initial + (no -> debitImpl(initial(no), amount))

    case r @ Credited(no, amount, next) => 
      initial + (no -> creditImpl(initial(no), amount))
  }
}

object Commands extends Commands

trait Commands {
  import Event._
  import scala.language.implicitConversions

  type Command[A] = Free[Event, A]

  private implicit def liftEvent[Next](event: Event[Next]): Command[Next] = liftF(event)

  def open(no: String, name: String, openingDate: Option[Date]): Command[String] = Opened(no, name, openingDate, identity)
  def close(no: String, closeDate: Option[Date]): Command[Unit] = Closed(no, closeDate, ())
  def debit(no: String, amount: Amount): Command[Unit] = Debited(no, amount, ())
  def credit(no: String, amount: Amount): Command[Unit] = Credited(no, amount, ())
}

object Scripts extends Commands {

  def transfer(from: String, to: String, amount: Amount): Command[Unit] = for {
    _ <- debit(from, amount)
    _ <- credit(to, amount)
  } yield ()

  val composite =
    for {
      n <- open("a-123", "debasish ghosh", Some(today))
      _ <- credit(n, 10000)
      _ <- credit(n, 30000)
      _ <- debit(n, 23000)
    } yield (())

  val compositeFail =
    for {
      n <- open("a-124", "debasish ghosh", Some(today))
      _ <- credit(n, 10000)
      _ <- credit(n, 30000)
      _ <- debit(n, 50000)
    } yield (())
}

object PureInterpreter {
  import Event._

  def interpret[A](c: Free[Event, A], state: Map[String, Account] = Map.empty): Map[String, Account] = c.resume.fold({
    case o @ Opened(no, name, odate, onInit) =>
      interpret(onInit(no), updateState(o, state)) 

    case c @ Closed(no, cdate, next) => 
      interpret(next, updateState(c, state))

    case d @ Debited(no, amount, next) => 
      interpret(next, updateState(d, state))

    case r @ Credited(no, amount, next) => 
      interpret(next, updateState(r, state))
  }, _ => state)

}

