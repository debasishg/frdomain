package frdomain.ch6
package task
package repository
package interpreter

import java.util.Date
import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import \/._
import model._

trait AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = MMap.empty[String, Account]
  lazy val ccyBalanceRepo = MMap.empty[(String, Date), Seq[Balance]]
  lazy val equityBalanceRepo = MMap.empty[(String, Date), Seq[Balance]]
  lazy val fixedIncomeBalanceRepo = MMap.empty[(String, Date), Seq[Balance]]

  def query(no: String): \/[NonEmptyList[String], Option[Account]] = repo.get(no).right
  def store(a: Account): \/[NonEmptyList[String], Account] = {
    val r = repo += ((a.no, a))
    a.right
  }
  def query(openedOn: Date): \/[NonEmptyList[String], Seq[Account]] = repo.values.filter(_.dateOfOpen == openedOn).toSeq.right
  def all: \/[NonEmptyList[String], Seq[Account]] = repo.values.toSeq.right

  def getCurrencyBalance(no: String, asOf: Date): String \/ Seq[Balance] = 
    ccyBalanceRepo.get((no, asOf)).getOrElse(Seq.empty[Balance]).right

  def getEquityBalance(no: String, asOf: Date): String \/ Seq[Balance] = 
    equityBalanceRepo.get((no, asOf)).getOrElse(Seq.empty[Balance]).right

  def getFixedIncomeBalance(no: String, asOf: Date): String \/ Seq[Balance] = 
    fixedIncomeBalanceRepo.get((no, asOf)).getOrElse(Seq.empty[Balance]).right
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory

