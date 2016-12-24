package frdomain.ch3
package solns

import org.scalacheck._
import Prop.{ forAll, BooleanOperators }
import Gen._
import Arbitrary.arbitrary

import lens._

/**
 * Exercise 3.2 Verifying Lens Laws
 *
 * The online code repository for chapter 3 contains a definition for a Customer entity and the 
 * lenses for it. Take a look at addressLens, which updates the address of a customer, and write 
 * properties using ScalaCheck that verify the laws of a lens.
 */
object LensLaws extends AddressLenses with CustomerLenses {
  import Lens._

  // generators for the various fields of Address
  val genNo = Gen.oneOf("B-12", "C-17", "D-80")
  val genStreet = Gen.oneOf("Monroe Street", "San Juan Street", "Camac Street")
  val genCity = Gen.oneOf("denver", "san jose", "chicago")
  val genState = Gen.oneOf("CA", "IL", "CO")
  val genZip = Gen.oneOf("90876", "80231", "95075")

  // generate a valid address
  val validAddress = for {
    no <- genNo
    st <- genStreet
    ct <- genCity
    st <- genState
    zp <- genZip
  } yield Address(no, st, ct, st, zp)

  implicit val arbitraryAddress: Arbitrary[Address] = Arbitrary { validAddress }

  // generators for the various fields of Customer
  val genCustNo = Gen.oneOf(1, 2, 3)
  val genName = Gen.oneOf("john", "david", "mary")

  // generate a valid customer
  val validCustomer = for {
    no <- genCustNo
    nm <- genName
    ac <- arbitrary[Address]
  } yield Customer(no, nm, ac)

  implicit val arbitraryCustomer: Arbitrary[Customer] = Arbitrary { validCustomer }

  // check Identity Law
  val lensIdentityLawCheck = for {
    c <- arbitrary[Customer]
  } yield addressLens.set(c, addressLens.get(c)) == c

  // check Retention Law
  val lensRetentionLawCheck = for {
    c <- arbitrary[Customer]
    a <- arbitrary[Address]
  } yield (addressLens.get(addressLens.set(c, a)) == a)

  // check Double Set Law
  val lensDoubleSetLawCheck = for {
    c <- arbitrary[Customer]
    a <- arbitrary[Address]
    b <- arbitrary[Address]
  } yield (addressLens.get(addressLens.set(addressLens.set(c, a), b)) == b)
}

object LensLawsSpecification extends Properties("LensLaws") {
  import LensLaws._

  property("Checking Lens Identity Law") = forAll(lensIdentityLawCheck)(_ == true)
  property("Checking Lens Retention Law") = forAll(lensRetentionLawCheck)(_ == true)
  property("Checking Lens Double Set Law") = forAll(lensDoubleSetLawCheck)(_ == true)
}
