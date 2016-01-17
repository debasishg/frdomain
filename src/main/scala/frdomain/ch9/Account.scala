package frdomain.ch9
package domain

import org.joda.time.DateTime
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scalaz._
import Scalaz._
import Kleisli._

case class Account(no: String, name: String, idNo: String, dateOpened: DateTime, dateClosed: Option[DateTime])


trait AccountService {
  type Error = String
  type ErrorOr[A] = Error \/ A

  def open(no: String, name: String, idNo: String, dateOpened: DateTime): ErrorOr[Account] = {
    val isValid: Boolean = verifyId(idNo, name)
    if (isValid) Account(no, name, idNo, dateOpened, None).right
    else s"Id ($idNo) validation failed".left
  }

  def verifyId(idNo: String, name: String): Boolean = {
    // possibly an expensive call that needs to communicate
    // with external systems, maybe over a Web service
    // stubbed here
    true
  }

  trait IdVerifier {
    def verifyId(idNo: String, name: String): Boolean
  }

  case class IdVerification() extends IdVerifier {
    def verifyId(idNo: String, name: String): Boolean = {
      //..
      true
    }
  }

  def open1(no: String, name: String, idNo: String, dateOpened: DateTime): IdVerifier => ErrorOr[Account] = { (v: IdVerifier) =>
    if (v.verifyId(idNo, name)) Account(no, name, idNo, dateOpened, None).right
    else s"Id ($idNo) validation failed".left
  }

  def open2(no: String, name: String, idNo: String, dateOpened: DateTime) = kleisli[ErrorOr, IdVerifier, Account] { (v: IdVerifier) =>
    if (v.verifyId(idNo, name)) Account(no, name, idNo, dateOpened, None).right
    else s"Id ($idNo) validation failed".left
  }
}
