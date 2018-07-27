package frdomain.ch6
package domain
package service
package interpreter

import cats._
import cats.data._
import cats.instances.all._
import cats.effect.IO

import repository.AccountRepository
import common._


class ReportingServiceInterpreter extends ReportingService[Amount] {

  def balanceByAccount: ReportOperation[Seq[(String, Amount)]] = Kleisli[Valid, AccountRepository, Seq[(String, Amount)]] { (repo: AccountRepository) =>
    EitherT {
      repo.all.map {
        case Left(errs) => Left(MiscellaneousDomainExceptions(errs))
        case Right(as) => Right(as.map(a => (a.no, a.balance.amount)))
      }
    }
  }
} 

object ReportingService extends ReportingServiceInterpreter
