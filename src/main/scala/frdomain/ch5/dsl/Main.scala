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
    a <- query(no).freek[PRG].onionT[O]
    s <- store(f(a)).freek[PRG].onionT[O]
  } yield s

  val dsl = for {
    a <- open("n123456", "debasish ghosh", today).freek[PRG].onionT[O]
    _ <- Log.info(s"Opening account $a").freek[PRG].onionT[O]
    t <- transact(a, BigDecimal(1000), today).freek[PRG].onionT[O]
    _ <- Log.info(s"Transacting on account $a").freek[PRG].onionT[O]
    _ <- store(t).freek[PRG].onionT[O]
    _ <- query(t.no).freek[PRG].onionT[O]
    s <- update(t.no, _.copy(name = "xxx"))
  } yield s

  val interpreters = accountOperationsDSL :&: accountRepositoryDSL :&: Log.logger 
  dsl.value.interpret(interpreters)
}
