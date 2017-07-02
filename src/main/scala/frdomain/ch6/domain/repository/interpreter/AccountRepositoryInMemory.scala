package frdomain.ch6
package domain
package repository
package interpreter

import java.util.Date

import frdomain.ch6.domain.model.Account
import frdomain.ch6.domain.service.Valid
import frdomain.ch6.domain.service.Valid._

import scala.collection.mutable.{Map => MMap}
import scalaz.Scalaz._

trait AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = MMap.empty[String, Account]

  override def query(no: String): Valid[Option[Account]] = repo.get(no).pure[Valid]

  override def store(a: Account): Valid[Account] = {
    repo += ((a.no, a))
    a.pure[Valid]
  }

  override def query(openedOn: Date): Valid[Seq[Account]] =
    repo.values.filter(_.dateOfOpen == openedOn).toSeq.pure[Valid]

  override def all: Valid[Seq[Account]] = repo.values.toSeq.pure[Valid]
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory

