package frdomain.ch6
package task
package app

import scalaz._
import Scalaz._
import Kleisli._
import scalaz.concurrent.Task
import Task._

import service.interpreter.PortfolioService
import repository.interpreter.AccountRepositoryInMemory
import model._
import common._

object Main {

  import PortfolioService._

  val accountNo = "a-123"
  val asOf = today

  val ccyPF: Task[Seq[Balance]] = getCurrencyPortfolio(accountNo, asOf)(AccountRepositoryInMemory)
  val eqtPF: Task[Seq[Balance]] = getEquityPortfolio(accountNo, asOf)(AccountRepositoryInMemory)
  val fixPF: Task[Seq[Balance]] = getFixedIncomePortfolio(accountNo, asOf)(AccountRepositoryInMemory)

  val r = Task.gatherUnordered(Seq(ccyPF, eqtPF, fixPF))
  val portfolio = CustomerPortfolio(accountNo, asOf, r.unsafePerformSync.foldLeft(List.empty[Balance])(_ ++ _))
}

