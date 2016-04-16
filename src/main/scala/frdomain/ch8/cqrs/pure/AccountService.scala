package frdomain.ch8
package cqrs
package pure

import java.util.Date

import scalaz.{~>, State, Free}
import common._

sealed trait Event[A]
case class Opened(no: String, name: String, openingDate: Option[Date]) extends Event[String]
case class Closed(no: String, closeDate: Option[Date]) extends Event[Unit]
case class Debited(no: String, amount: Amount) extends Event[Unit]
case class Credited(no: String, amount: Amount) extends Event[Unit]

object Event {

  private def debitImpl(a: Account, amount: Amount) = {
    if (a.balance.amount < amount) throw new RuntimeException("insufficient fund to debit")
    a.copy(balance = Balance(a.balance.amount - amount))
  }

  private def creditImpl(a: Account, amount: Amount) = {
    a.copy(balance = Balance(a.balance.amount + amount))
  }

  def updateState(e: Event[_], initial: Map[String, Account]) = e match {
    case Opened(no, name, odate) =>
      initial + (no -> Account(no, name, odate.get))

    case Closed(no, cdate) =>
      initial + (no -> initial(no).copy(dateOfClosing = cdate))

    case Debited(no, amount) =>
      initial + (no -> debitImpl(initial(no), amount))

    case Credited(no, amount) =>
      initial + (no -> creditImpl(initial(no), amount))
  }
}

object Commands extends Commands

trait Commands {
  import Event._
  import scala.language.implicitConversions

  type Command[A] = Free[Event, A]

  private implicit def liftEvent[A](event: Event[A]): Command[A] = Free.liftF(event)

  def open(no: String, name: String, openingDate: Option[Date]): Command[String] = Opened(no, name, openingDate)
  def close(no: String, closeDate: Option[Date]): Command[Unit] = Closed(no, closeDate)
  def debit(no: String, amount: Amount): Command[Unit] = Debited(no, amount)
  def credit(no: String, amount: Amount): Command[Unit] = Credited(no, amount)
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
  import Commands.Command
  type MapState[A] = State[Map[String, Account], A]

  val step = new (Event ~> MapState) {
    override def apply[A](fa: Event[A]): MapState[A] = fa match {
      case o @ Opened(no, _, _) =>
        State { s => (updateState(o, s), no) }

      case c @ Closed(_, _) =>
        State { s => (updateState(c, s), ()) }

      case d @ Debited(_, _) =>
        State { s => (updateState(d, s), ()) }

      case r @ Credited(_, _) =>
        State { s => (updateState(r, s), ()) }
    }
  }

  def interpret[A](c: Command[A], state: Map[String, Account] = Map.empty): Map[String, Account] =
    c.foldMap(step).exec(state)

}

