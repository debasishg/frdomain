package frdomain.ch5
package solns

import scala.language.higherKinds
import scala.collection.mutable.{ Map => MMap }
import scalaz.{ Store => _, _ }
import Scalaz._
import scalaz.concurrent.Task
import Task.{now, fail}

import free._

/**
 * Exercise 5.1: Effects in Interpreters
 */
object EffectInterpreter {
  // the Effect is now an input to the interpreter. Here we are interpreting the
  // free monad into a Task. We could interpret it to some other effect with this
  // strategy
  trait AccountRepoInterpreter[M[_]] {
    def apply[A](action: AccountRepo[A]): M[A]
  }
  
  // State monad keeps the state - hence the interpreter does not expose a mutable Map
  object AccountRepoState {
    type AccountMap = Map[String, Account]
  
    type Err[A] = String \/ A
    type AcccountState[A] = StateT[Err, AccountMap, A]
  }
  
  object AccountRepoStateInterpreter extends AccountRepoInterpreter[AccountRepoState.AcccountState] {
    import AccountRepoState._
  
    val step: AccountRepoF ~> AcccountState = new (AccountRepoF ~> AcccountState) {
      override def apply[A](fa: AccountRepoF[A]): AcccountState[A] = fa match {
        case Query(no) =>
          StateT[Err, AccountMap, Account] { table =>
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
  
    def apply[A](action: AccountRepo[A]): AcccountState[A] = action.foldMap(step)
  
    private def modifyState(f: AccountMap => AccountMap): AcccountState[Unit] =
      StateT[Err, AccountMap, Unit] { table =>
        (f(table), ()).right
      }
  }
}
