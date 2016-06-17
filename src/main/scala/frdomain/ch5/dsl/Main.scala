package frdomain.ch5
package dsl

import org.joda.time.DateTime
import cats.data.{ Xor, NonEmptyList }
import freek._

object Main {
  import common._
  import Account._
  import AccountOperations._

  type PRG = AccountOperations.AccountOps :|: Log.DSL :|: FXNil

  type O = Xor[String, ?] :&: Bulb

  val dsl = for {
    a <- AccountOperations.open("n123456", "debasish ghosh", today).freek[PRG].onionT[O]
    _ <- Log.info(s"Opening account $a").freek[PRG].onionT[O]
    t <- AccountOperations.transact(a, BigDecimal(1000), today).freek[PRG].onionT[O]
    _ <- Log.info(s"Transacting on account $a").freek[PRG].onionT[O]
  } yield t

  val interpreters = AccountOperations.accountOperationsDSL :&: Log.logger 
  dsl.value.interpret(interpreters)
}
