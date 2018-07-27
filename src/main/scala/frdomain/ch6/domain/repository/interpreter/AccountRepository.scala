package frdomain.ch6
package domain
package repository
package interpreter

import java.util.Date
import scala.collection.mutable.{ Map => MMap }

import cats._
import cats.data._
import cats.instances.all._
import cats.effect.IO

import common._
import model.{ Account, Balance }

trait AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = MMap.empty[String, Account]

  def query(no: String): IO[ErrorOr[Option[Account]]] = IO(Right(repo.get(no)))

  def store(a: Account): IO[ErrorOr[Account]] = IO {
    val r = repo += ((a.no, a))
    Right(a)
  }

  def query(openedOn: Date): IO[ErrorOr[Seq[Account]]] = IO {
    Right(repo.values.filter(_.dateOfOpen == openedOn).toSeq)
  }

  def all: IO[ErrorOr[Seq[Account]]] = IO(Right(repo.values.toSeq))
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory
