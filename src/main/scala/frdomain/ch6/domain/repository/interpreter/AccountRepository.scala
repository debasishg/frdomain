package frdomain.ch6
package domain
package repository
package interpreter

import java.util.Date
import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import \/._
import model.{ Account, Balance }

trait AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = MMap.empty[String, Account]

  def query(no: String): \/[NonEmptyList[String], Option[Account]] = repo.get(no).right
  def store(a: Account): \/[NonEmptyList[String], Account] = {
    val r = repo += ((a.no, a))
    a.right
  }
  def query(openedOn: Date): \/[NonEmptyList[String], Seq[Account]] = repo.values.filter(_.dateOfOpen == openedOn).toSeq.right
  def all: \/[NonEmptyList[String], Seq[Account]] = repo.values.toSeq.right
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory

