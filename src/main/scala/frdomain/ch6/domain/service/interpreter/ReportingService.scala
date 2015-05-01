package frdomain.ch6
package domain
package service
package interpreter

import scalaz._
import Scalaz._
import Kleisli._

import scala.concurrent._
import ExecutionContext.Implicits.global

import repository.AccountRepository
import model.common._


class ReportingServiceInterpreter extends ReportingService[Amount] {

  def balanceByAccount: ReportOperation[Seq[(String, Amount)]] = kleisli[Valid, AccountRepository, Seq[(String, Amount)]] { (repo: AccountRepository) =>
    EitherT {
      Future {
        repo.all match {
          case \/-(as) => as.map(a => (a.no, a.balance.amount)).right
          case a @ -\/(_) => a
        }
      }
    }
  }
} 

object ReportingService extends ReportingServiceInterpreter
