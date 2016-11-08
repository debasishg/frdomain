package frdomain.ch3
package algebra
package interpreter

import org.scalacheck._
import Prop.forAll

object AccountServiceSpec extends Properties("AccountService") {

  import AccountService._

  val genValidAccountNo = Gen.choose(100000, 999999).map(_.toString)
  val genName = Gen.oneOf("john", "david", "mary")

  val validOpenedAccount = for {
    no <- genValidAccountNo
    name <- genName
  } yield open(no, name, None)

  property("open account has no timing issues") =
    forAll(validOpenedAccount)(_.isSuccess)

}
