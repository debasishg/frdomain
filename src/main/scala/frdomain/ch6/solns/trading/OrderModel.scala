package frdomain.ch6
package solns
package trading

import java.util.{ Date, Calendar }
import scalaz._
import Scalaz._

trait OrderModel {this: RefModel =>
  case class LineItem(ins: Instrument, qty: BigDecimal, price: BigDecimal)
  case class Order(no: String, date: Date, customer: Customer, items: List[LineItem])

  type ClientOrder = Map[String, String]

  def fromClientOrders: List[ClientOrder] => Valid[Order] = { cos =>
    ListT[StringOr, Order](
      cos.map {co =>
        val ins = co("instrument").split("-")
        val lineItems = ins map {in =>
          val arr = in.split("/")
          LineItem(arr(0), BigDecimal(arr(1)), BigDecimal(arr(2)))
        }
        makeOrder(co, lineItems.toList)
      }.sequenceU
    )
  }

  private def makeOrder(m: Map[String, String], lis: List[LineItem]): String \/ Order = (for {
    n <- m.get("no")
    c <- m.get("customer")
  } yield Order(n, Calendar.getInstance.getTime, c, lis)).cata(_.right[String], "Error making Order".left[Order])
}
