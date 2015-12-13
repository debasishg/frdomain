package frdomain.e2e
package domain

import scalaz._
import Scalaz._

import Common._

/**
 * Currency and supported currencies
 */
sealed trait Currency
case object USD extends Currency
case object AUD extends Currency
case object INR extends Currency
case object JPY extends Currency
case object GBP extends Currency
case object EUR extends Currency

/**
 * We model `Money` as a `Map` so that we can consider an amount of money as comprised of
 * multiple currencies e.g. Bob has money worth 5 USD + 2000 JPY, which can be modeled
 * easily using this abstraction.
 */
case class Money(amount: Map[Currency, Amount]) {
  def toBaseCurrency: Amount = ???
}

object Money {
  final val zeroMoney: Money = Money(Monoid[Map[Currency, BigDecimal]].zero)

  /**
   * We define a monoid over addition of `Money`
   */
  implicit val MoneyAdditionMonoid = new Monoid[Money] {
    def zero: Money = zeroMoney
    def append(m1: Money, m2: => Money) = Money(m1.amount |+| m2.amount)
  }
}
