package frdomain.ch5
package free

import scala.language.higherKinds
import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._

trait AccountRepoInterpreter[M[_]] {
  def apply[A](action: AccountRepo[A]): M[A]
}
  
/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class AccountRepoMutableInterpreter() extends AccountRepoInterpreter[Task] {
  val table: MMap[String, Account] = MMap.empty[String, Account]

  def step[A](action: AccountRepoF[AccountRepo[A]]): Task[AccountRepo[A]] = action match {

    case Query(no, onResult) =>
      table.get(no).map { a => now(onResult(a)) }
                   .getOrElse { fail(new RuntimeException(s"Account no $no not found")) }

    case Store(account, next) => now(table += ((account.no, account))).map { _ => next }
    case Delete(no, next) => now(table -= no).map { _ => next }
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: AccountRepo[A]): Task[A] = action.runM(step)
}

object AccountRepoState {
  type AMap = Map[String, Account]

  type Err[A] = String \/ A
  type ErrState[A] = StateT[Err, AMap, A]
}

object AccountRepoStateInterpreter extends AccountRepoInterpreter[AccountRepoState.ErrState] {
  import AccountRepoState._

  def step[A](action: AccountRepoF[AccountRepo[A]]): ErrState[AccountRepo[A]] = action match {

    case Query(no, onResult) =>
      StateT[Err, AMap, Account] { table =>
        table.get(no) match {
          case Some(account) => (table, account).right
          case None => s"Account no $no not found".left
        }
      }.map(onResult)
    case Store(account, next) =>
      modifyState(_ + (account.no -> account)).map { _ => next }
    case Delete(no, next) =>
      modifyState(_ - no).map { _ => next }
  }

  def apply[A](action: AccountRepo[A]): ErrState[A] = action.runM(step)

  private def modifyState(f: AMap => AMap): ErrState[Unit] =
    StateT[Err, AMap, Unit] { table =>
      (f(table), ()).right
    }
}

case class AccountRepoShowInterpreter() {

  def interpret[A](script: AccountRepo[A], ls: List[String]): List[String] = script.fold(_ => ls, {

    case Query(no, onResult) =>
      interpret(onResult(Account(no, "")), ls ++ List(s"Query for $no"))

    case Store(account, next) =>
      interpret(next, ls ++ List(s"Storing $account"))

    case Delete(no, next) =>
      interpret(next, ls ++ List(s"Deleting $no"))

  })

}
