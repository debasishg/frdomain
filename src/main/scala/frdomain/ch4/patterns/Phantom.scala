package frdomain.ch4
package patterns

import scala.language.higherKinds
import java.util.{ Date, Calendar }

object Loans {

  import scalaz._
  import Scalaz._

  val today = Calendar.getInstance.getTime

  case class LoanApplication[Status] private[Loans](
    date: Date,
    name: String,
    purpose: String,
    repayIn: Int,
    actualRepaymentYears: Option[Int] = None,
    startDate: Option[Date] = None,
    loanNo: Option[String] = None,
    emi: Option[BigDecimal] = None
  )

  trait Applied
  trait Approved
  trait Enriched
  trait Disbursed

  type LoanApplied = LoanApplication[Applied]
  type LoanApproved = LoanApplication[Approved]
  type LoanEnriched = LoanApplication[Enriched]
  type LoanDisbursed = LoanApplication[Disbursed]

  def applyLoan(name: String, purpose: String, repayIn: Int, date: Date = today) =
    LoanApplication[Applied](date, name, purpose, repayIn)

  def approve = Kleisli[Option, LoanApplied, LoanApproved] { l => 
    l.copy(
      loanNo = scala.util.Random.nextString(10).some,
      actualRepaymentYears = 15.some,
      startDate = today.some
    ).some.map(identity[LoanApproved])
  }

  def enrich = Kleisli[Option, LoanApproved, LoanEnriched] { l => 
    val x = for {
      y <- l.actualRepaymentYears
      s <- l.startDate
    } yield (y, s)

    l.copy(emi = x.map { case (y, s) => calculateEMI(y, s) }).some.map(identity[LoanEnriched])
  }

  private def calculateEMI(tenure: Int, startDate: Date): BigDecimal = BigDecimal(0)

  val l = applyLoan("debasish", "house building", 10)
  val op = approve andThen enrich

  op run l

  // val nop = enrich andThen approve
  // doesn't compile
}
