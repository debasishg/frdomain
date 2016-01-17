package frdomain.ch9
package domain.model

import java.util.Date
import org.scalacheck._
import Prop.{ forAll, BooleanOperators }
import Gen._
import Arbitrary.arbitrary

import scalaz._
import Scalaz._

object AllGen {

  import common._

  implicit val arbitraryBalance: Arbitrary[Balance] = Arbitrary {
    for {
      b <- arbitrary[Amount] suchThat (a => a > 0 && a < 100)
    } yield Balance(b)
  }

  val genValidAccountNo = Gen.oneOf("123456", "23456789", "34567890")
  val genInvalidAccountNo = Gen.oneOf("1234", "23")
  val genName = Gen.oneOf("john", "david", "mary")
  def genOptionalCloseDate(seed: Date) = Gen.oneOf(Some(new Date(seed.getTime + 10000)), None)

}

object CheckingAccountSpecification extends Properties("Account") {

  import Account._
  import AllGen._

  val genCheckingAccountCreation = for {
    no <- genValidAccountNo
    nm <- genName
    od <- arbitrary[Date]
    cd <- genOptionalCloseDate(od)
    bl <- arbitrary[Balance]
  } yield checkingAccount(no, nm, Some(od), cd, bl)

  val genSavingsAccountCreation = for {
    no <- genValidAccountNo
    nm <- genName
    rt <- Gen.choose(5, 10)
    od <- arbitrary[Date]
    bl <- arbitrary[Balance]
  } yield savingsAccount(no, nm, rt, Some(od), None, bl)

  property("Create Checking Account") = forAll(genCheckingAccountCreation) { a =>
    a.isRight == true
  } 

  property("Create Savings Account") = forAll(genSavingsAccountCreation) { a =>
    a.isRight == true
  } 

  property("Close if not already closed") = forAll(genCheckingAccountCreation) { creation =>
    creation.isRight == true
    creation.map { account =>
      account.dateOfClose.map(_ => true).getOrElse(
        close(account, account.dateOfOpen.map (d => new Date(d.getTime + 100000)).getOrElse(common.today)).isRight == true
      )
    }.getOrElse(false)
  }
}
