package frdomain.ch4
package patterns

import scala.language.higherKinds
import java.util.{ Date, Calendar }

import Monoid._
import scalaz.State
import State._

object States {
  type AccountNo = String
  type BS = Map[AccountNo, Balance]
  val balances: BS = Map(
    "a1" -> Balance(),
    "a2" -> Balance(),
    "a3" -> Balance(),
    "a4" -> Balance(),
    "a5" -> Balance()
  )

  def updateBalance(txns: List[Transaction]) = modify { (b: BS) => 
    txns.foldLeft(b) { (a, txn) =>
      implicitly[Monoid[BS]].op(a, Map(txn.accountNo -> Balance(txn.amount))) 
    }
  }

  case class Transaction(accountNo: AccountNo, amount: Money)

  val txns: List[Transaction] = List(
    Transaction("a1", Money(Map(USD -> BigDecimal(100)))),
    Transaction("a2", Money(Map(USD -> BigDecimal(100)))),
    Transaction("a1", Money(Map(INR -> BigDecimal(500000)))),
    Transaction("a3", Money(Map(USD -> BigDecimal(100)))),
    Transaction("a2", Money(Map(AUD -> BigDecimal(200))))
  )

  updateBalance(txns) run balances
}

