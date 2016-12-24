package frdomain.ch6
package solns
package trading

import scalaz._
import Scalaz._

/**
 * Exercise 6.1: Handling Effects Algebraically
 */
trait Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  def clientOrders: Kleisli[Valid, List[ClientOrder], Order]
  def execute(market: Market, brokerAccount: Account): Kleisli[Valid, Order, Execution]
  def allocate(accounts: List[Account]): Kleisli[Valid, Execution, Trade]

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]) = {
    clientOrders               andThen    
    execute(market, broker)    andThen   
    allocate(clientAccounts)
  }
}
