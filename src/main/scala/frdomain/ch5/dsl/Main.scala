package frdomain.ch5
package dsl

import common._
import Account.AccountValidationNel
import cats.data.Xor
import freek._

object Main extends AccountOperations with AccountRepository {

  type PRG = AccountOps :|: AccountRepo :|: Log.DSL :|: FXNil
  type O = AccountValidationNel :&: Xor[String, ?] :&: Bulb

  def update(no: String, f: Account => Account) = for {
    a <- query(no).freeko[PRG, O]
    s <- store(f(a)).freeko[PRG, O]
  } yield s

  val dsl = for {
    a <- open("n123456", "debasish ghosh", today).freeko[PRG, O]
    _ <- Log.info(s"Opening account $a").freeko[PRG, O]
    t <- transact(a, BigDecimal(1000), today).freeko[PRG, O]
    _ <- Log.info(s"Transacting on account $a").freeko[PRG, O]
    _ <- store(t).freeko[PRG, O]
    _ <- query(t.no).freeko[PRG, O]
    s <- update(t.no, _.copy(name = "xxx"))
  } yield s

  val interpreters = accountOperationsInterpreter :&: accountRepositoryInterpreter :&: Log.logger 
  dsl.value.interpret(interpreters)
}
