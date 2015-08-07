package frdomain.ch4
package trading

import java.util.Date

trait OrderModel {this: RefModel =>
  case class LineItem(ins: Instrument, qty: BigDecimal, price: BigDecimal)
  case class Order(no: String, date: Date, customer: Customer, items: List[LineItem])

  case class ClientOrder(details: Map[String, String])

  def fromClientOrders: ClientOrder => List[Order] = { cos =>
    List(Order("123", today, "abc", List.empty[LineItem]))
  }
}
