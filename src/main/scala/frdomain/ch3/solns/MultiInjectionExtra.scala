package frdomain.ch3
package solns

import java.util.Date
import util.{ Try, Success, Failure }
import repository._
import common._

import scalaz._
import Scalaz._

/**
 * Exercise 3.3 (variant):  Injecting multiple dependencies
 *                In this section’s example, you injected a repository into a domain service by using 
 *                the Reader monad. In many cases, you’ll need to inject several dependen- cies into your 
 *                domain service. For example, you may need to inject multiple repositories or another service 
 *                or some configuration parameters as dependen- cies to a domain service.
 *
 *                Suggest a suitable strategy for handling injection of multiple dependencies in AccountService. 
 *                One way could be to combine all dependencies into a single environment type and then pass it as a single dependency.
 *
 * We implement an optimization over MultiInjection. We can create a composite Config object,
 * but not all APIs need that. An API may need only a subset of the objects to be injected. This solution
 * demonstrates how we can compose such APIs.
 *
 * Note: we need some specialized functionality in Reader, for which we use the Scalaz variant.
 */
object MultiInjectionExtra {
  trait VerificationService {
    def verifyCustomerProfile(ssn: String): Try[Boolean]
  }

  case class Config(accountRepo: AccountRepository, verificationService: VerificationService)

  trait AccountService[Account, Amount, Balance] {
    def open(no: String, ssn: String, name: String, openingDate: Option[Date]): Reader[Config, Try[Account]]
    def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]]
    def debit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
    def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
    def balance(no: String): Reader[AccountRepository, Try[Balance]]
  }
  
  object AccountService extends AccountService[Account, Amount, Balance] {
  
    // open needs the while Config object as it uses both of the constituent members
    def open(no: String, ssn: String, name: String, openingDate: Option[Date]) = Reader { (c: Config) =>
      c.verificationService.verifyCustomerProfile(ssn) match {
        case Success(true) => doOpen(no, name, openingDate, c.accountRepo)
        case Success(false) => Failure(new Exception(s"Profile verification failed"))
        case Failure(ex) => Failure(new Exception(s"Verification service unavailable. Please try later"))
      }
    }

    private def doOpen(no: String, name: String, openingDate: Option[Date], 
      accountRepo: AccountRepository): Try[Account] = accountRepo.query(no) match {
        case Success(Some(a)) => Failure(new Exception(s"Already existing account with no $no"))
        case Success(None) =>
          if (no.isEmpty || name.isEmpty) Failure(new Exception(s"Account no or name cannot be blank") )
          else if (openingDate.getOrElse(today) before today) Failure(new Exception(s"Cannot open account in the past"))
          else accountRepo.store(Account(no, name, openingDate.getOrElse(today)))
        case Failure(ex) => Failure(new Exception(s"Failed to open account $no: $name", ex))
      }
  
    // Needs only AccountRepository to be inejcted
    def close(no: String, closeDate: Option[Date]) = Reader { (repo: AccountRepository) => 
      repo.query(no) match {
        case Success(Some(a)) =>
          if (closeDate.getOrElse(today) before a.dateOfOpening) 
            Failure(new Exception(s"Close date $closeDate cannot be before opening date ${a.dateOfOpening}")) 
          else repo.store(a.copy(dateOfClosing = closeDate))
        case Success(None) => Failure(new Exception(s"Account not found with $no"))
        case Failure(ex) => Failure(new Exception(s"Fail in closing account $no", ex))
      }
    }
  
    // Needs only AccountRepository to be inejcted
    def debit(no: String, amount: Amount) = Reader { (repo: AccountRepository) =>
      repo.query(no) match {
        case Success(Some(a)) =>
          if (a.balance.amount < amount) Failure(new Exception("Insufficient balance"))
          else repo.store(a.copy(balance = Balance(a.balance.amount - amount)))
        case Success(None) => Failure(new Exception(s"Account not found with $no"))
        case Failure(ex) => Failure(new Exception(s"Fail in debit from $no amount $amount", ex))
      }
    }
  
    // Needs only AccountRepository to be inejcted
    def credit(no: String, amount: Amount) = Reader { (repo: AccountRepository) =>
      repo.query(no) match {
        case Success(Some(a)) => repo.store(a.copy(balance = Balance(a.balance.amount + amount)))
        case Success(None) => Failure(new Exception(s"Account not found with $no"))
        case Failure(ex) => Failure(new Exception(s"Fail in credit to $no amount $amount", ex))
      }
    }
  
    // Needs only AccountRepository to be inejcted
    def balance(no: String) = Reader((repo: AccountRepository) => repo.balance(no))

    // A sample workflow composite method that needs to compose over multiple
    // methods that need subsets of Config to be injected
    def workflow(no: String): Reader[Config, Try[Balance]] = for {
      a <- open("a1", "ssn-1", "john", None)
      c <- credit(no, BigDecimal(100)).local { (cf: Config) => cf.accountRepo }
      b <- balance(no).local { (cf: Config) => cf.accountRepo }
    } yield b
  }
}

