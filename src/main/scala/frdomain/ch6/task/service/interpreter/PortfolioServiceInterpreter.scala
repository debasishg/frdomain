package frdomain.ch6
package task
package service
package interpreter

import java.util.{ Date, Calendar }

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import \/._
import Kleisli._

import model._
import model.common._
import repository.AccountRepository

class PortfolioServiceInterpreter extends PortfolioService {
  def getCurrencyPortfolio(no: String, 
    asOf: Date) = kleisli[Task, AccountRepository, Seq[Balance]] { (repo: AccountRepository) =>

    Task {
      repo.getCurrencyBalance(no, asOf) match {
        case \/-(b) => b
        case -\/(_) => throw new Exception(s"Failed to fetch currency balance")
      }
    }
  }

  def getEquityPortfolio(no: String, 
    asOf: Date) = kleisli[Task, AccountRepository, Seq[Balance]] { (repo: AccountRepository) =>

    Task {
      repo.getEquityBalance(no, asOf) match {
        case \/-(b) => b
        case -\/(_) => throw new Exception(s"Failed to fetch equity balance")
      }
    }
  }

  def getFixedIncomePortfolio(no: String, 
    asOf: Date) = kleisli[Task, AccountRepository, Seq[Balance]] { (repo: AccountRepository) =>

    Task {
      repo.getFixedIncomeBalance(no, asOf) match {
        case \/-(b) => b
        case -\/(_) => throw new Exception(s"Failed to fetch fixed income balance")
      }
    }
  }
}

object PortfolioService extends PortfolioServiceInterpreter
