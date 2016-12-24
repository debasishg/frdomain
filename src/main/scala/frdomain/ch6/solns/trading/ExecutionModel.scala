package frdomain.ch6
package solns
package trading

import scalaz._
import Scalaz._

trait ExecutionModel {this: RefModel with OrderModel =>
  case class Execution(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal)

  def executionsFromOrder(market: Market, brokerAccount: Account): Order => Valid[Execution] = { order =>
    ListT[StringOr, Execution](
      order.items.map { item =>
        Execution(brokerAccount, item.ins, "e-123", market, item.price, item.qty).right[String]
      }.sequenceU
    )
  }
}

