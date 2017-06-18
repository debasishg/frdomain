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

  def balanceByAccount: ReportOperation[Seq[(String, Amount)]] =
    kleisli[Valid, AccountRepository, Seq[(String, Amount)]] { (repo: AccountRepository) =>
      repo.all map { as => as.map(a => (a.no, a.balance.amount)) }
    }
}

object ReportingService extends ReportingServiceInterpreter
