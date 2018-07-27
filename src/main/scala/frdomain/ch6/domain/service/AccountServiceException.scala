package frdomain.ch6
package domain
package service

import cats._
import cats.data._
import cats.instances.all._

trait AccountServiceException {
  def message: NonEmptyList[String]
}

case class AlreadyExistingAccount(no: String) extends AccountServiceException {
  val message = NonEmptyList.of(s"Already existing account with no $no")
}

case class NonExistingAccount(no: String) extends AccountServiceException {
  val message = NonEmptyList.of(s"No existing account with no $no")
}

case class ClosedAccount(no: String) extends AccountServiceException {
  val message = NonEmptyList.of(s"Account with no $no is closed")
}

case object RateMissingForSavingsAccount extends AccountServiceException {
  val message = NonEmptyList.of("Rate needs to be given for savings account")
}

case class MiscellaneousDomainExceptions(msgs: NonEmptyList[String]) extends AccountServiceException {
  val message = msgs
}

