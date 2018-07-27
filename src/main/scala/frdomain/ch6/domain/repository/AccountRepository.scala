package frdomain.ch6
package domain
package repository

import java.util.Date

import cats._
import cats.data._
import cats.instances.all._

import cats.effect.IO
import common._

import model.{ Account, Balance }

trait AccountRepository { 
  def query(no: String): IO[ErrorOr[Option[Account]]]
  def store(a: Account): IO[ErrorOr[Account]]
  def query(openedOn: Date): IO[ErrorOr[Seq[Account]]]
  def all: IO[ErrorOr[Seq[Account]]]

  def balance(no: String): IO[ErrorOr[Balance]] = query(no).map {
    case Right(Some(a)) => Right(a.balance)
    case Right(None) => Left(NonEmptyList.of(s"No account exists with no $no"))
    case Left(x) => Left(x)
  }
}
