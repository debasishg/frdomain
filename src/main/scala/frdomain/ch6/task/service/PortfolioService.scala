package frdomain.ch6
package task
package service

import java.util.Date
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Kleisli._

import scala.language.higherKinds
import repository.AccountRepository
import model._

trait PortfolioService {
  type PFOperation[A] = Kleisli[Task, AccountRepository, Seq[A]]

  def getCurrencyPortfolio(no: String, asOf: Date): PFOperation[Balance]
  def getEquityPortfolio(no: String, asOf: Date): PFOperation[Balance]
  def getFixedIncomePortfolio(no: String, asOf: Date): PFOperation[Balance]
}


