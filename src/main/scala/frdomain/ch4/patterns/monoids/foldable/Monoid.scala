package frdomain.ch4
package patterns
package monoids.foldable

trait Monoid[T] {
  def zero: T
  def op(t1: T, t2: T): T
}

object Monoid {

  def apply[T](implicit monoid: Monoid[T]) = monoid
  
  implicit val IntAdditionMonoid = new Monoid[Int] {
    val zero = 0
    def op(i: Int, j: Int) = i + j
  }
  
  implicit val BigDecimalAdditionMonoid = new Monoid[BigDecimal] {
    val zero = BigDecimal(0)
    def op(i: BigDecimal, j: BigDecimal) = i + j
  }
  
  implicit def MapMonoid[K, V: Monoid] = new Monoid[Map[K, V]] {
    def zero = Map.empty[K, V]
    def op(m1: Map[K, V], m2: Map[K, V]) = m2.foldLeft(m1) { (a, e) =>
      val (key, value) = e
      a.get(key).map(v => a + ((key, implicitly[Monoid[V]].op(v, value)))).getOrElse(a + ((key, value)))
    }
  }

  final val zeroMoney: Money = Money(Monoid[Map[Currency, BigDecimal]].zero)

  implicit def MoneyAdditionMonoid = new Monoid[Money] {
    val m = implicitly[Monoid[Map[Currency, BigDecimal]]]
    def zero = zeroMoney
    def op(m1: Money, m2: Money) = Money(m.op(m1.m, m2.m))
  }

  object MoneyOrdering extends Ordering[Money] {
    def compare(a:Money, b:Money) = a.toBaseCurrency compare b.toBaseCurrency
  }

  import MoneyOrdering._
  import scala.math.Ordering

  implicit val MoneyCompareMonoid = new Monoid[Money] {
    def zero = zeroMoney
    def op(m1: Money, m2: Money) = if (gt(m1, m2)) m1 else m2
  }
}
