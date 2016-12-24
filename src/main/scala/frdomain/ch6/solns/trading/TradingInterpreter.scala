package frdomain.ch6
package solns
package trading

import java.util.{ Date, Calendar }
import scalaz.{ Order => OrderZ, _ }
import Scalaz._
import Kleisli._

import TradeModel._

trait TradingInterpreter extends Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  def clientOrders = kleisli[Valid, List[ClientOrder], Order] { fromClientOrders }

  def execute(market: Market, brokerAccount: Account) = kleisli[Valid, Order, Execution] { executionsFromOrder(market, brokerAccount) }

  def allocate(accounts: List[Account]) = kleisli[Valid, Execution, Trade] { allocateCustomerTrades(accounts) }
}

object TradingInterpreter extends TradingInterpreter

