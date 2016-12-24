package frdomain.ch6
package solns

import scalaz._

package object trading {
  type StringOr[A] = String \/ A
  type Valid[A] = ListT[StringOr, A]
}
