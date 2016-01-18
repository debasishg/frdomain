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

  val genAmount = for {
    value <- Gen.chooseNum(100, 10000000)
    valueDecimal = BigDecimal.valueOf(value)
  } yield valueDecimal / 100
 
  val genBalance = genAmount map Balance

  implicit val arbitraryBalance: Arbitrary[Balance] = Arbitrary { genBalance }

  val genValidAccountNo = Gen.choose(100000, 999999).map(_.toString)
  val genInvalidAccountNo = Gen.choose(1000, 9999).map(_.toString)

  val genName = Gen.oneOf("john", "david", "mary")

  def genOptionalCloseDate(seed: Date) = 
    Gen.frequency(
      (8, Some(aDateAfter(seed))), 
      (1, None)
    )
  def aDateAfter(date: Date) = new Date(date.getTime() + 10000)
  def aDateBefore(date: Date) = new Date(date.getTime() - 10000)
  def genInvalidOptionalCloseDate(seed: Date) = Gen.oneOf(Some(aDateBefore(seed)), None)
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
    cd <- genOptionalCloseDate(od)
    bl <- arbitrary[Balance]
  } yield savingsAccount(no, nm, rt, Some(od), cd, bl)

  val genFailedCheckingAccountCreation = for {
    no <- genInvalidAccountNo
    nm <- genName
    od <- arbitrary[Date]
    cd <- genInvalidOptionalCloseDate(od)
    bl <- arbitrary[Balance]
  } yield checkingAccount(no, nm, Some(od), cd, bl)

  val genClosedCheckingAccountCreation = for {
    no <- genValidAccountNo
    nm <- genName
    od <- arbitrary[Date]
    cd <- genOptionalCloseDate(od) suchThat (_ isDefined)
    bl <- arbitrary[Balance]
  } yield checkingAccount(no, nm, Some(od), cd, bl)

  val genZeroBalanceCheckingAccountCreation = for {
    no <- genValidAccountNo
    nm <- genName
    od <- arbitrary[Date]
  } yield checkingAccount(no, nm, Some(od), None, Balance(0))

  property("Checking Account creation successful") = forAll(genCheckingAccountCreation)(_.isRight) 

  property("Checking Account creation failure") = forAll(genFailedCheckingAccountCreation)(_.isLeft)

  property("Savings Account creation successful") = forAll(genSavingsAccountCreation)(_.isRight)

  property("Close Account if not already closed") = forAll(genCheckingAccountCreation) { creation =>
    creation.isRight == true
    creation.map { account =>
      account.dateOfClose.map(_ => true).getOrElse(
        close(account, 
              account.dateOfOpen.map(aDateAfter(_)).getOrElse(common.today)
        ).isRight == true
      )
    }.getOrElse(false)
  }

  property("Update balance on closed account fails") = forAll(genClosedCheckingAccountCreation, genAmount) { (creation, amount) =>
    creation.isRight == true
    creation.map { account =>
      updateBalance(account, amount).isLeft == true
    }.getOrElse(false)
  }

  property("Update balance on account with insufficient funds fails") = forAll(genZeroBalanceCheckingAccountCreation, genAmount) { (creation, amount) =>
    creation.isRight == true
    creation.map { account =>
      updateBalance(account, -amount).isLeft == true
    }.getOrElse(false)
  }
}
