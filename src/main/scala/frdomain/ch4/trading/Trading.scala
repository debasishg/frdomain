package frdomain.ch4
package trading

import scalaz._
import Scalaz._

trait Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  def clientOrders: Kleisli[List, List[ClientOrder], Order]
  def execute(market: Market, brokerAccount: Account): Kleisli[List, Order, Execution]
  def allocate(accounts: List[Account]): Kleisli[List, Execution, Trade]

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]) = {
    clientOrders               andThen    
    execute(market, broker)    andThen   
    allocate(clientAccounts)
  }
}
