package frdomain.ch5
package free

import scala.language.higherKinds
import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task.{now, fail}

trait AccountRepoInterpreter[M[_]] {
  def apply[A](action: AccountRepo[A]): M[A]
}

/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class AccountRepoMutableInterpreter() extends AccountRepoInterpreter[Task] {
  val table: MMap[String, Account] = MMap.empty[String, Account]

  val step: AccountRepoF ~> Task = new (AccountRepoF ~> Task) {
    override def apply[A](fa: AccountRepoF[A]): Task[A] = fa match {
      case Query(no) =>
        table.get(no).map { a => now(a) }
                     .getOrElse { fail(new RuntimeException(s"Account no $no not found")) }

      case Store(account) => now(table += ((account.no, account))).void
      case Delete(no) => now(table -= no).void
    }
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: AccountRepo[A]): Task[A] = action.foldMap(step)
}

object AccountRepoState {
  type AMap = Map[String, Account]

  type Err[A] = String \/ A
  type ErrState[A] = StateT[Err, AMap, A]
}

object AccountRepoStateInterpreter extends AccountRepoInterpreter[AccountRepoState.ErrState] {
  import AccountRepoState._

  val step: AccountRepoF ~> ErrState = new (AccountRepoF ~> ErrState) {
    override def apply[A](fa: AccountRepoF[A]): ErrState[A] = fa match {
      case Query(no) =>
        StateT[Err, AMap, Account] { table =>
          table.get(no) match {
            case Some(account) => (table, account).right
            case None => s"Account no $no not found".left
          }
        }
      case Store(account) =>
        modifyState(_ + (account.no -> account))
      case Delete(no) =>
        modifyState(_ - no)
    }
  }

  def apply[A](action: AccountRepo[A]): ErrState[A] = action.foldMap(step)

  private def modifyState(f: AMap => AMap): ErrState[Unit] =
    StateT[Err, AMap, Unit] { table =>
      (f(table), ()).right
    }
}

case class AccountRepoShowInterpreter() {

  type ListState[A] = State[List[String], A]
  val step: AccountRepoF ~> ListState = new (AccountRepoF ~> ListState) {
    private def show(s: String): ListState[Unit] = State(l => (l ++ List(s), ()))
    override def apply[A](fa: AccountRepoF[A]): ListState[A] = fa match {
      case Query(no) => show(s"Query for $no").map(_ => Account(no, ""))
      case Store(account) => show(s"Storing $account")
      case Delete(no) => show(s"Deleting $no")
    }
  }

  def interpret[A](script: AccountRepo[A], ls: List[String]): List[String] =
    script.foldMap(step).exec(ls)

}
