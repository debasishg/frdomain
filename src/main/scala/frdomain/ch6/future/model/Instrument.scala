package frdomain.ch6
package future
package model

import java.util.Date
import common._

sealed trait InstrumentType

case object CCY extends InstrumentType
case object EQ extends InstrumentType
case object FI extends InstrumentType

sealed trait Instrument {
  def instrumentType: InstrumentType
}

case class Equity(isin: String, name: String, issueDate: Date, faceValue: Amount) extends Instrument {
  final val instrumentType = EQ
}

case class FixedIncome(isin: String, name: String, issueDate: Date, maturityDate: Option[Date], 
  nominal: Amount) extends Instrument {
  final val instrumentType = FI
}

case class Currency(isin: String) extends Instrument {
  final val instrumentType = CCY
}
