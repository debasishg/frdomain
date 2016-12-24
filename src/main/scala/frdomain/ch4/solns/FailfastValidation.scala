package frdomain.ch4
package solns

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }
import scalaz._
import Scalaz._

/**
 *
 * Exercise 4.3: Failfast Validation (Monadic)
 */
object FailfastValidation {
  // utilities
  object common {
    type Amount = BigDecimal
  
    def today = Calendar.getInstance.getTime
  }
  
  import common._
  
  case class Balance(amount: Amount = 0)
  
  // contract for Account
  sealed trait Account {
    def no: String
    def name: String
    def dateOfOpen: Option[Date]
    def dateOfClose: Option[Date]
    def balance: Balance
  }
  
  // checking account
  final case class CheckingAccount (no: String, name: String,
    dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account
  
  // savings account
  final case class SavingsAccount (no: String, name: String, rateOfInterest: Amount, 
    dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account
  
  object Account {
  
    // validation functions : each returns a \/[String, _]
    private def validateAccountNo(no: String): String \/ String = 
      if (no.isEmpty || no.size < 10) 
        s"Account No has to be at least 10 characters long: found $no".left
      else no.right
  
    private def validateOpenCloseDate(od: Date, cd: Option[Date]): String \/ (Option[Date], Option[Date]) = cd.map { c => 
      if (c before od) 
        s"Close date [$c] cannot be earlier than open date [$od]".left
      else (od.some, cd).right
    }.getOrElse { (od.some, cd).right }
  
    private def validateRate(rate: BigDecimal): String \/ BigDecimal =
      if (rate <= BigDecimal(0)) s"Interest rate $rate must be > 0".left
      else rate.right
  
    // the validations are invoked monadically
    // E \/ A in scalaz defines a Monad by virtue of which the function
    // stops on encountering the first failure
    def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date], 
      closeDate: Option[Date], balance: Balance): String \/ Account = { 
  
      val od = openDate.getOrElse(today)

      for {
        n <- validateAccountNo(no) 
        d <- validateOpenCloseDate(openDate.getOrElse(today), closeDate)
        r <- validateRate(rate)
      } yield SavingsAccount(n, name, r, d._1, d._2, balance)
    }
  }
}  

/**
Welcome to Scala 2.12.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_91).
Type in expressions for evaluation. Or try :help.

scala> import frdomain.ch4.solns._
import frdomain.ch4.solns._

scala> import FailfastValidation._
import FailfastValidation._

scala> import Account._
import Account._

scala> savingsAccount("1234", "john", BigDecimal(0.1), None, None, Balance())
res0: scalaz.\/[String,frdomain.ch4.solns.FailfastValidation.Account] = -\/(Account No has to be at least 10 characters long: found 1234)

scala> savingsAccount("1234", "john", BigDecimal(-0.1), None, None, Balance())
res1: scalaz.\/[String,frdomain.ch4.solns.FailfastValidation.Account] = -\/(Account No has to be at least 10 characters long: found 1234)

scala> savingsAccount("123456789000", "john", BigDecimal(0.1), None, None, Balance())
res2: scalaz.\/[String,frdomain.ch4.solns.FailfastValidation.Account] = \/-(SavingsAccount(123456789000,john,0.1,Some(Mon Dec 26 07:51:02 IST 2016),None,Balance(0)))
**/
