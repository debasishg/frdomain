package frdomain.ch6
package future
package service
package interpreter

import java.util.{ Date, Calendar }

import scalaz._
import Scalaz._
import \/._
import Kleisli._

import scala.concurrent._
import ExecutionContext.Implicits.global

import model._
import model.common._
import repository.AccountRepository

class PortfolioServiceInterpreter extends PortfolioService {
  def getCurrencyPortfolio(no: String, 
    asOf: Date) = kleisli[Future, AccountRepository, Seq[Balance]] { (repo: AccountRepository) =>

    Future {
      repo.getCurrencyBalance(no, asOf) match {
        case \/-(b) => b
        case -\/(_) => throw new Exception(s"Failed to fetch currency balance")
      }
    }
  }

  def getEquityPortfolio(no: String, 
    asOf: Date) = kleisli[Future, AccountRepository, Seq[Balance]] { (repo: AccountRepository) =>

    Future {
      repo.getEquityBalance(no, asOf) match {
        case \/-(b) => b
        case -\/(_) => throw new Exception(s"Failed to fetch equity balance")
      }
    }
  }

  def getFixedIncomePortfolio(no: String, 
    asOf: Date) = kleisli[Future, AccountRepository, Seq[Balance]] { (repo: AccountRepository) =>

    Future {
      repo.getFixedIncomeBalance(no, asOf) match {
        case \/-(b) => b
        case -\/(_) => throw new Exception(s"Failed to fetch fixed income balance")
      }
    }
  }
}

object PortfolioService extends PortfolioServiceInterpreter
