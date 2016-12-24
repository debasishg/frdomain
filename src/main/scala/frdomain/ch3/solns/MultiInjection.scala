package frdomain.ch3
package solns

import java.util.Date
import util.{ Try, Success, Failure }
import repository._
import repository.reader._
import common._

/**
 * Exercise 3.3:  Injecting multiple dependencies
 *                In this section’s example, you injected a repository into a domain service by using 
 *                the Reader monad. In many cases, you’ll need to inject several dependen- cies into your 
 *                domain service. For example, you may need to inject multiple repositories or another service 
 *                or some configuration parameters as dependen- cies to a domain service.
 *
 *                Suggest a suitable strategy for handling injection of multiple dependencies in AccountService. 
 *                One way could be to combine all dependencies into a single environment type and then pass it as a single dependency.
 *
 * The solution is to create an abstraction containing all dependencies that need to be injected. And
 * then use it within the Reader monad.
 *
 * There can be an interesting variation on this strategy when you can optimize by having APIs
 * that need part of the composite object to be injected. We discuss this in MultiInjectionExtra.
 */
object MultiInjection {
  // dummay service that also need to be injected
  trait VerificationService {
    def verifyCustomerProfile(ssn: String): Try[Boolean]
  }

  // composite config containing all things that you need to inject
  case class Config(accountRepo: AccountRepository, verificationService: VerificationService)

  // Note all methods that Config to be injected. This may need finer grained
  // handling which we discuss in MultiInjectionExtra
  trait AccountService[Account, Amount, Balance] {
    def open(no: String, ssn: String, name: String, openingDate: Option[Date]): Reader[Config, Try[Account]]
    def close(no: String, closeDate: Option[Date]): Reader[Config, Try[Account]]
    def debit(no: String, amount: Amount): Reader[Config, Try[Account]]
    def credit(no: String, amount: Amount): Reader[Config, Try[Account]]
    def balance(no: String): Reader[Config, Try[Balance]]
  }
  
  object AccountService extends AccountService[Account, Amount, Balance] {
  
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
  
    def close(no: String, closeDate: Option[Date]) = Reader { (c: Config) => 
      c.accountRepo.query(no) match {
        case Success(Some(a)) =>
          if (closeDate.getOrElse(today) before a.dateOfOpening) 
            Failure(new Exception(s"Close date $closeDate cannot be before opening date ${a.dateOfOpening}")) 
          else c.accountRepo.store(a.copy(dateOfClosing = closeDate))
        case Success(None) => Failure(new Exception(s"Account not found with $no"))
        case Failure(ex) => Failure(new Exception(s"Fail in closing account $no", ex))
      }
    }
  
    def debit(no: String, amount: Amount) = Reader { (c: Config) =>
      c.accountRepo.query(no) match {
        case Success(Some(a)) =>
          if (a.balance.amount < amount) Failure(new Exception("Insufficient balance"))
          else c.accountRepo.store(a.copy(balance = Balance(a.balance.amount - amount)))
        case Success(None) => Failure(new Exception(s"Account not found with $no"))
        case Failure(ex) => Failure(new Exception(s"Fail in debit from $no amount $amount", ex))
      }
    }
  
    def credit(no: String, amount: Amount) = Reader { (c: Config) =>
      c.accountRepo.query(no) match {
        case Success(Some(a)) => c.accountRepo.store(a.copy(balance = Balance(a.balance.amount + amount)))
        case Success(None) => Failure(new Exception(s"Account not found with $no"))
        case Failure(ex) => Failure(new Exception(s"Fail in credit to $no amount $amount", ex))
      }
    }
  
    def balance(no: String) = Reader((c: Config) => c.accountRepo.balance(no))
  }
}
