package frdomain.ch3
package solns

import java.util.Date

object OptionBased {

  /**
   * Exercise 3.1: Define AccountService with Option instead of Try as the return type
   *               of the APIs. The strategy is simple - as mentioned in the text, Option
   *               is a monad as well and has a flatMap method. Hence just replace Try
   *               with Option and we are done.
   *
   *               Do you think Option is a better way to model the use case instead of Try ?
   *               The Option data type abstracts optionality - it's used to abstract over
   *               data types that may or may not have a value. In our use case the return type
   *               of each API also needs to report failures along with the actual values. And
   *               the user will obviously look for reasons of failure as well. Option doesn't
   *               give you this ability directly. In case of failures, you can return None but
   *               no other detail. Hence it's not a recommended option - use Try or Either.
   */
  trait AccountService[Account, Amount, Balance] {
    def open(no: String, name: String, openingDate: Option[Date]): Option[Account]
    def close(account: Account, closeDate: Option[Date]): Option[Account]
    def debit(account: Account, amount: Amount): Option[Account]
    def credit(account: Account, amount: Amount): Option[Account]
    def balance(account: Account): Option[Balance]
  
    def transfer(from: Account, to: Account, amount: Amount): Option[(Account, Account, Amount)] = for {
      a <- debit(from, amount)
      b <- credit(to, amount)
    } yield (a, b, amount)
  }
}

object EitherBased {

  /**
   * Exercise 3.1: Define AccountService with Either instead of Try as the return type
   *               of the APIs. As long as we have a monad we can implement the chnage
   *               quite seamlessly. In Scala 2.12, Either implements flatMap and hence we
   *               we can simply replace Try with Either. The general convention is to have the
   *               failure as the left projection and we implement it as a String here. Feel free
   *               to make failure reporting more meaningful with your own ADT in place of String.
   *               Hence just replace Try with Either and we are done.
   *
   *               Do you think Either is a better way to model the use case instead of Try ?
   *
   *               Either is a more pure abstraction from functional programming point of view. Besides
   *               the fact that Try violates one of the fucntor laws (https://issues.scala-lang.org/browse/SI-6284),
   *               the entire implementation of Try is based on handling of exceptions. This has lots of
   *               values when you interoperate with a library that throws exceptions (usually many Java libraries
   *               fall in this category). But when you are developing your own abstractions from ground up,
   *               you have no reason to use exceptions for control flow or failure reporting. Under such
   *               circumstances, Scala offers better alternatives. Either, specifically the 2.12 version,
   *               offers one of the better ways to handle such scenarios. It offers better integration
   *               with custom ADTs for error reporting - you can use custom data types to encapsulate error
   *               details instead of String and Either offers better integration of such data structures. Also
   *               Either has better exhaustivity checking. Travis Brown nicely describes each of these in
   *               this StackOverflow post (http://stackoverflow.com/a/31405256).
   */
  type AccountServiceStatus[A] = Either[String, A]

  trait AccountService[Account, Amount, Balance] {
    def open(no: String, name: String, openingDate: Option[Date]): AccountServiceStatus[Account]
    def close(account: Account, closeDate: Option[Date]): AccountServiceStatus[Account]
    def debit(account: Account, amount: Amount): AccountServiceStatus[Account]
    def credit(account: Account, amount: Amount): AccountServiceStatus[Account]
    def balance(account: Account): AccountServiceStatus[Balance]
  
    def transfer(from: Account, to: Account, amount: Amount): AccountServiceStatus[(Account, Account, Amount)] = for {
      a <- debit(from, amount)
      b <- credit(to, amount)
    } yield (a, b, amount)
  }
}
