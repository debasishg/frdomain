package frdomain.ch3
package lens

case class Address(no: String, street: String, city: String, state: String, zip: String)
case class Customer(id: Int, name: String, address: Address)

trait AddressLenses {
  protected val noLens = Lens[Address, String](
    get = _.no,
    set = (o, v) => o.copy(no = v)
  )

  protected val streetLens = Lens[Address, String](
    get = _.street,
    set = (o, v) => o.copy(street = v)
  )

  protected val cityLens = Lens[Address, String](
    get = _.city,
    set = (o, v) => o.copy(city = v)
  )

  protected val stateLens = Lens[Address, String](
    get = _.state,
    set = (o, v) => o.copy(state = v)
  )

  protected val zipLens = Lens[Address, String](
    get = _.zip,
    set = (o, v) => o.copy(zip = v)
  )
}

trait CustomerLenses {
  protected val nameLens = Lens[Customer, Int](
    get = _.id,
    set = (o, v) => o.copy(id = v)
  )

  protected val addressLens = Lens[Customer, Address](
    get = _.address,
    set = (o, v) => o.copy(address = v)
  )
}

object App extends AddressLenses with CustomerLenses {
  import Lens._

  val a = Address(no = "B-12", street = "Monroe Street", city = "Denver", state = "CO", zip = "80231")
  val c = Customer(12, "John D Cook", a)

  val custAddrNoLens = compose(addressLens, noLens)
  custAddrNoLens.get(c)
  custAddrNoLens.set(c, "B675")

}
