package frdomain.ch5
package dsl

import common._
import Account.AccountValidationNel
import cats.data.Xor
import freek._

object Main extends AccountOperations with AccountRepository {

  type PRG = Log.DSL :|: AccountOps.PRG :||: AccountRepo.PRG
  type O = AccountValidationNel :&: Xor[String, ?] :&: Bulb

  val dsl = for {
    a <- open("n123456", "debasish ghosh", today).freeko[PRG, O]
    _ <- Log.info(s"Opening account $a").freeko[PRG, O]
    t <- transact(a, BigDecimal(1000), today).freeko[PRG, O]
    _ <- Log.info(s"Transacting on account $a").freeko[PRG, O]
    _ <- store(t).freeko[PRG, O]
    _ <- query(t.no).freeko[PRG, O]
    s <- update(t.no, _.copy(name = "xxx")).freeko[PRG, O]
  } yield s

  val interpreters = accountOperationsInterpreter :&: accountRepositoryInterpreter :&: Log.logger 
  dsl.value.interpret(interpreters)
}
